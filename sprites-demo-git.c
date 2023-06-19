// demo goal : show a demo screen as for ikaruga with at least 100 bullet sprites in motion
// with two bullets per sprite, max possible is like 8*9*2 = 144
// if we have char rows as well we could stretch to 200 with 3 sets of 20 char rows so we could
// even target a 200 bullet demo. for starters just the sprite version...
//
// two possible sprite approaches, both based on the idea of a "series" of bullets using the same sprite
// repeated in a sprite column.
//
// one approach : tie series raster frequency to sprite reuse frequency
//   e.g. bullet height = 9 pixels; + 3 blank; +9 pixels for second bullet; gap of 3 pixels to sprite reuse
//   gives a frequency of 12 rasters with exactly 2 bullets per sprite
//
// second approach : sprites redisplay every 21 rasters independent of series raster frequency. bullets
//   need to have sprite display at many different phases relative to sprite (ideally 21; maybe fewer)
//   e.g. bullet height = 12 pixels; + 3 blank. series update frequency 15 rasters. natural 7 phases.
//
// limitations:
//    bullets in same series need to have y separation
//    bullets in same series need to have same y speed
//    for this demo all series have same update frequency
//
// for this demo we have may have all bullets with the same image. this limitation would need to be relaxed.
// 
// challenge is compute. we have a budget of maybe 100 cycles per bullet per frame
//    100 bullets * 100 cycles = 10000 cycles;
//    312*63 = 19656 cycles per frame (pal)
//   -210*18 =  3780 cycles eaten by full sprites DMA
//    -25*42 =  1050 cycles eaten by char DMA (unless we dodge it)
//           = 14826 cyles for everything
//    -10*80 =   800 cycles approx min for sprite reuse triggering
// 
//
// thinking multiple frequencies. suppose frequency 9,12,15. Arrange sprite updates for frequency 9 to
// happen at raster = 0 mod 3; for frequency 12 at raster = 1 mod 3; for 15 at raster = 2 mod 3; updates
// at same frequency not on same raster. Then at most one update required per raster.
//
// 
// try to do demo at frequency 15 rasters.
// 1.5 rasters per series for onscreen updates. includes sprite x movement via dx addition,
// sprite y movement via sp table evolution, sprite x positioning with hi bit, y positioning via sp,
// hit table check for oob and target hitting
// 1.5*8 = 12 rasters of 15 frequency. +2 badline, +1 sprite y refresh.
// with all sprites on, ~45 cycles per raster. 1.5*45 = 67.

// below is 56 cycles. 56*8 = 448 = 44*10+8. 
//
// y position : determined relative to current physical sprite by spriteptr
//    physical sprite (mod 21) set by centromere y
//
// hit logic:
//    side border : disable bullet? ignore?
//    bullet on player : kill or absorb
//    enemy on player bullet : damage
//    enemy on player : kill?
//    bullet on obstacle?
//
// s1x[15] : coarse x position (ring buffer)
// s1dx[15]
// s1p[15]
// *s1offp

// s0p[x],..,s7p[x] gives base pose definition for object
// s0offp,..,s6offp gives modified pose
//    for bullets with position determined by preshifted sprites,
//      changing s1offp is used to shift the column up/down
//      s1offp[s1p[x]] is the preshifted sprite image for correct y offset
//      this combines y shift baked into s1p[x] with y shift for column, and can account for up/down shift
//      of the physical sprite column as well as encoding movement of the whole column
//    for ship sprites embedded in the bullet column
//      s1offp[ship]=ship : lookup should be skipped if code is specific to ship display
//      assumes physical sprites are positioned to put ship sprite on physical sprite boundary
//      assumes update raster code timed to put updated on +/- ship start boundary
//    for background sprite
//      no need for s7offp : movement controlled by sprite column, not preshifting

//    lda (boundscheck),y  //; 5  
//    and s1vuln,x         //; 4
//    bne *+1              //; 2 / 3
//    nop3 stx s1bullethit //; 4 / 3
//
// can save one cycle with separate runouts ...
//    beq +                // 2 / 3
//    stx s1bullethit      // 3 / 0
//  +:
//
// possible to combine s1p, s1vuln?
// boundscheck takes value 0 or BCBULLET while player bullet active
// boundscheck takes value 0 or BCSHIP while player ship active
// we depend on vuln AND BCBULLET nonzero iff enemy ship active and overlap
// we depend on voln AND BCSHIP nonzero iff enemy bullet or ship and overlap
// s1p can have bit 7 set iff it is enemy ship
// s1p can have bit 7 clear, nonzero value iff it is enemy bullet
// then BCBULLET = 128, BCSHIP = 255
//   => s1p AND BCBULLET nonnzero iff enemy ship active and overlap
//   => s1p AND BCSHIP nonzero iff enemy bullet or ship and overlap
//
//          ship bull nada
// ship      s1p  128   -
// bullet    s1p   -    -
// nada      -     -    -
//


void __align(0x100) series_code[] = kickasm
  {{
// ; v_revisit_1
    lda s1x,x            //; 4    
    adc s1dx,x           //; 4  // assert carry clear 
    sta s1x,x            //; 5
    tay                  //; 2
// ; 15 .. updated s1x (saved in y for bounds check) [11 bytes]
    asl           //; 2
    sta sprite1x  //; 4 [xlo on cycle 21] 
    lda spritehi  //; 4
    bcs +         //; 2 / 3
    and #ff-2     //; 2 / 0
    bcc ++        //; 3 / 0
    +: ora #$02   //; 0 / 2
    clc           //; 0 / 2
    ++: sta spritehi  //; 4 [xhi on cycle 36]
// ; 21 .. adjusted sprite1x,spritehi 
    lda (boundscheck),y  //; 5  
    and s1p,x            //; 4
    bne *+1              //; 2 / 3
    nop3 stx s1bullethit //; 4 / 3
// ; 15 .. did bounds check and stored collision in s1bullethit
    ldy s1p,x            //; 4 
    lda (s1offp),y       //; 5
    sta sprite1p_scr1    //; 4
    sta sprite1p_scr2    //; 4
// ; 17 adjusted sprite1p  [xx bytes]
// ; total constant 68 cycles (71 with single jumpout) [xx bytes]
}};

void __align(0x100) series_code[] = kickasm
  {{
// ; v_revisit_1_b
    ldy s1p,x            //; 4 
    lda (s1offp),y       //; 5
    sta sprite1p_scr1    //; 4
    sta sprite1p_scr2    //; 4
// ; 17 adjusted sprite1p  [xx bytes]
    lda s1x,x            //; 4   
    adc s1dx,x           //; 4 // assert carry already clear 
    sta s1x,x            //; 5
    tay                  //; 2
// ; 15 .. updated s1x (saved in y for bounds check) [11 bytes]
    asl           //; 2
    sta sprite1x  //; 4 
    lda spritehi  //; 4
    bcc clearhi   //; 2 / 3
    clc           //; 2 / 0
    ora #02       //; 2 / 0
    sta spritehi  //; 4 / 0   
//; 20 .. adjusted spritehi [case setlo]
 bchk:
    lda (boundscheck),y //; 5   / 0
    and s1p,x           //; 4   / 0
    beq jmpoutp2        //; 2\3 / 0
 stxjmpout:
    stx s1bullethit     //; 3\0 / 0\3
    jmp selfmodnext
// 14+jumpout .. bounds check
 clearhi:   
    and #ff-2           //; 0 / 2
    sta spritehi        //; 0 / 4 [xhi on cycle xx]
// 19 .. adjusted spritehi [case clearhi]
    lda (boundscheck),y  //; 0 / 5  
    and s1p,x            //; 0 / 4
    bne stxjmpout        //; 0 / 2\3
    nop                  //; 0 / 2\0
 jmpout2:
    nop                 //; 0\2 / 2\0
    jmp selfmodnext
// 15 + jumpout .. bounds check [case clearhi]
// ; total constant 69 cycles with double jumpout 
//
// ; assumes boundscheck[2], s1bullethit[7], s1offp[14] on zp
// ; assumes sprite1p_scr2, 2*jumpout target are modified by external code as needed
}};

// dx based code : 15 cycles update x
//    lda s1x,x            //; 4   
//    adc s1dx,x           //; 4 // assert carry already clear 
//    sta s1x,x            //; 5
//    tay                  //; 2
//
// pattern based code : 11 cycles update x 
//    ldy s1patx,x         //; 4
//    lda (s1patxoff),y    //; 5
//    tay                  //; 2
//
// switch to pattern based x would also mean we don't need to keep carry clear, should save 1 cycle after re-arrange
// so that would give constant 64 cycles with jumpout.
// requires offset tables say for each dx, only permits one dx per series (or could be interleaved with code dup)
//

void __align(0x100) series_code[] = kickasm
  {{
// ; v_revisit_playersprite0_shipvsbullet
    lda s0x,x      //; 4
    eor #$ff       //; 2
    sta boundscheck_lo //; 3
    eor #$ff       //; 2
// ; 11 .. constant s0x, adjusted boundscheck 
    asl           //; 2
    sta sprite0x  //; 4 [xlo on cycle ] 
    lda spritehi  //; 4
    bcs +         //; 2 / 3
    and #$ff-#$1  //; 2 / 0
    bcc ++        //; 3 / 0
    +: ora #$1    //; 0 / 2
    clc           //; 0 / 2
    ++: sta spritehi  //; 4 [xhi on cycle]
// ; 21 .. adjusted sprite1x,spritehi 
    ldy s0p,x            //; 4
    bpl +                //; 2 / 3
    lda #BOUNDSHIP_HI    //; 2+1 / 0
    sta boundscheck_hi   //; 3 / 0
    sty sprite1p_scr1    //; 4 / 0
    sty sprite1p_scr2    //; 4 / 0
    jmp selfmod_next
    +:
    lda (s0offp),y       //; 0 / 5    
    sta sprite1p_scr1    //; 0 / 4
    sta sprite1p_scr2    //; 0 / 4
    jmp selfmod_next
// ; 20 adjusted sprite1p  [xx bytes]
// ; total constant 52 cycles + 3 jumpout
// assumes boundscheck_hi is set to BOUNDBULLET_HI in the interframe
// assumes s0x is set to an offscreen position for inactive bullets
//    * for vertically overlapped shot stream can have s0p update to just head or tail when one is removed
//    * then s0x updates to offscreen when both are removed
}};


void __align(0x100) series_code[] = kickasm
  {{
// ; v_revisit_bgsprite7
    lda s7x,x            //; 4
// ; 4 .. constant s7x 
    asl           //; 2
    sta sprite7x  //; 4 [xlo on cycle 21] 
    lda spritehi  //; 4
    bcs +         //; 2 / 3
    and #$ff-#$80 //; 2 / 0
    bcc ++        //; 3 / 0
    +: ora #$80   //; 0 / 2
    clc           //; 0 / 2
    ++: sta spritehi  //; 4 [xhi on cycle 36]
// ; 21 .. adjusted sprite1x,spritehi 
    lda s7p,x            //; 4 
    sta sprite7p_scr1    //; 4
    sta sprite7p_scr2    //; 4
// ; 12 adjusted sprite1p (no offset as no preshifting)
// ; total constant 37 cycles +3 jumpout 
}};

//..........
void __align(0x100) series_code[] = kickasm
  {{
// ; v_revisit_playersprite0_shipvsbullet_and_bgsprite7
    ldy s7p,x            //; 4 
    sty sprite7p_scr1    //; 4
    sty sprite7p_scr2    //; 4 can move sty below to have better bunching
// ; 12 adjusted sprite1p (no offset as no preshifting)
    lda s0x,x            //; 4
    eor #$ff             //; 2
    sta boundscheck_lo   //; 3
    eor #$ff             //; 2
// ; 11 .. constant s0x, adjusted boundscheck 
    asl             //;  2
    sta sprite0x    //;  4 
    lda s7x,x       //;  4 
    bcc c0          //;  2  / 3         [+1 c0....]
// ; 12 [s0] / 13 [c0] .. set sprite0x      
    asl             //;  2  / 0
    sta sprite7x    //;  4  / 0
    lda spritehi    //;  4  / 0
    ora #$81        //;  2  / 0
    bcs c0s7_rejoin_s0s7 //; 2\3 / 0      
// ; entered case s0c7
    and zpconst_7f       //; 3\0 / 0    [+2 s0c7..]
    sta spritehi         //; 4\0 / 0
// ; 21 s0c7     

    ldy s0p,x            //;  4\0  / 0
    bpl bulletpose       //; 2;3\0 / 0  [+1 s0c7bp]
shippose:
    lda #BOUNDSHIP_HI    //; 2;0
    sta boundscheck_hi   //; 3;0
    sty sprite1p_scr1    //; 4;0
    sty sprite1p_scr2    //; 4;0
    jmp selfmod_next     //; 3
// ; 22 shippose [not including branch into]
    
c0:
    asl           //;  0 /  2
    sta sprite7x  //;  0 /  4
    lda spritehi  //;  0 /  4
    and #$7e      //;  0 /  2
    bcc c0c7      //;  0 / 2\3      
c0s7_rejoin_s0s7:
    ora #$80      //;  0\2 / 2\0      [+1 c0s7.. +2 s0s7..]
c0c7:
    sta spritehi  //;  0\4 / 4\4      
// ; 20 [c0s7] \ 19 [c0c7]
    
    ldy s0p,x            //; 4
    bmi shippose         //; 2;3      [+1 c0..sp +1 s0s7sp]
bulletpose:
    lda (s0offp),y       //; 0;5    
    sta sprite1p_scr1    //; 0;4
    sta sprite1p_scr2    //; 0;4
    jmp selfmod_next     //; 0;3
// ; 22 bulletpose [not including branch into]
    
// ; total 76+[1..3] = 77-79 including jumpout (? can make constant?)
// ; [
// ; have not guaranteed carry clear in above!
// ; compared 92 for the two original routines including one jumpout
}};




// hit logic ... 
// need to determine if player bullet is in hit box of an enemy
// need to determine if enemy bullet is in hit box of player
// may also have screen boundary or world object collision
// idea : separate code for slices with player ship vs player bullets
//    player bullet x-coordinate at s0p,x
//    have player series update modify a pointer playerbullethit
//        pointer is to an array of hit/don't hit.
//        low byte of playerbullethit = player bullet x-coordinte
//    if x-coordinate hit:
//       if hit is on enemy ship:
//          player bullet becomes a splat
//          enemy ship takes damage depending on bullet type          
//             1 damage matching color     
//             2 damage nonmatching color
//             [0 damage enemy invulnerable ? use to mask enemy bullet/player bullet collision?]
//       record index of hitting bullet and handle intraframe  
//       simple store records at most one hit per enemy per frame. could be okay anyway?
//
// checkhit:
//   lda s1vuln,x             ; 4
//   and (playerbullethit),y  ; 4
//   beq nohit                ; 2  +1
//   stx s1damage             ; +3  
//  nohit:
//  ; 13 cycles nohit, 11 cycles hit
//
//  we have hi/lo branch and hit/nohit branch.  hi/lo is +0/+1 ;
//  ____ +0 lo ____ +3 hit                                   [+3]
//    \          \_ ....... +1 nohit                         [+1]
//     \ +1 hi .......................____ +3 hit            [+4]
//                                      \_ ....... +1 nohit  [+2]
//
//  bne *+1              ; 2/3
//  nop3 stx s1damage    ; 4/3 : total 6 cycles constant
//
//  similarly for hi/lo:
//  ora #$01          ; 2
//  bcs *+1           ; 2 / 3
//  nop3 and $zpfe    ; 4 / 3 (zpfe contains fe as and #$fe is just 2 cycles) 
//  ; 8 cycles. a simpler double-branch version could be 7 so not strictly competitive
//
//  bcs +
//  ora #$01
//  bcc ++
// +:
//  and #$fe
//  clc
// ++:
//  ; 7 cycles. side effect is carry guaranteed clear after

#include <c64.h>

// Location of screen & sprites
byte* SCREEN = (byte *) 0x400;
byte* SPRITES_PTR = (byte *) 0x7f8;

char __align(0x100) YSIN[0x100] = kickasm {{
    .var min = 50
    .var max = 250-21
    .var ampl = max-min;
    .for(var i=0;i<256;i++)
        .byte round(min+(ampl/2)+(ampl/2)*sin(toRadians(360*i/256)))
}};

__address(0x2000) char SPRITE[] = kickasm(resource "balloon.png") {{
    .var pic = LoadPicture("balloon.png", List().add($000000, $ffffff))
    .for (var y=0; y<21; y++)
        .for (var x=0;x<3; x++)
            .byte pic.getSinglecolorByte(x,y)
}};

void main() {
    asm { sei }
    init();
    loop();
}

// Initialize the program
void init() {
    *D011 = VICII_DEN | VICII_RSEL | 3;
    word xp = 100;
    for(byte sx: 0..7) {
      SPRITES_XPOS[sx<<1] = (byte)xp;
      SPRITES_YPOS[sx<<1] = 30;
      SPRITES_COLOR[sx] = GREEN;
      SPRITES_PTR[sx] = (byte)(SPRITE/$40);
      xp += 24;
      *SPRITES_XMSB = 0x80;
    }
    // Enable & initialize sprites
    *SPRITES_ENABLE = $ff;
}

// The raster loop
void loop() {
    // The current index into the y-sinus
    byte sin_idx = 0;
    while(true) {
        while(*RASTER!=$ff) {}
        (*BORDER_COLOR)++;
        *BORDER_COLOR = BLACK;
	while(*RASTER!=1){}
	byte sy = 30;
	while(sy<234) {
	  for(byte sx: 0..7) {
	    SPRITES_YPOS[sx<<1] = sy;
	  }
	  while (*RASTER<=sy){}
	  sy += 21;
	} 
    }
}

