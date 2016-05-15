// Change this to be at least as long as your pixel string (too long will work fine, just be a little slower)

#define PIXELS 96*4  // Number of pixels in the string

// These values depend on which pins your 8 strings are connected to and what board you are using 
// More info on how to find these at http://www.arduino.cc/en/Reference/PortManipulation

// PORTD controls Digital Pins 0-7 on the Uno

// You'll need to look up the port/bit combination for other boards. 

// Note that you could also include the DigitalWriteFast header file to not need to to this lookup.

#define PIXEL_PORT  PORTD  // Port of the pin the pixels are connected to
#define PIXEL_DDR   DDRD   // Port of the pin the pixels are connected to

#define PIXEL_BITMASK 0b11111110  // If you do not want to use all 8 pins, you can mask off the ones you don't want
                                  // Note that these will still get 0 written to them when we send pixels
                                  // TODO: If we have time, we could even add a varibale that will and/or into the bits before writing to the port to support any combination of bits/values

// These are the timing constraints taken mostly from the WS2812 datasheets 
// These are chosen to be conservative and avoid problems rather than for maximum throughput 

#define T1H  900    // Width of a 1 bit in ns
#define T1L  600    // Width of a 1 bit in ns

#define T0H  400    // Width of a 0 bit in ns
#define T0L  900    // Width of a 0 bit in ns

#define RES 6000    // Width of the low gap between bits to cause a frame to latch

// Here are some convience defines for using nanoseconds specs to generate actual CPU delays

#define NS_PER_SEC (1000000000L)          // Note that this has to be SIGNED since we want to be able to check for negative values of derivatives

#define CYCLES_PER_SEC (F_CPU)

#define NS_PER_CYCLE ( NS_PER_SEC / CYCLES_PER_SEC )

#define NS_TO_CYCLES(n) ( (n) / NS_PER_CYCLE )

// Actually send the next set of 8 bits to the 8 pins.
// We must to drop to asm to enusre that the complier does
// not reorder things and make it so the delay happens in the wrong place.

// OnBits is the mask of which bits are being used (PIXEL_BITMASK). We pass it on so that we
// do not turn on unused pins becuae this would enable the pullup. Also, hopefully passing this
// will cause the compiler to allocate a Register for it and avoid a reload every pass.


// Send a full 8 bits down all the pins, represening a single color of 1 pixel
// We walk though the 8 bits in colorbyte one at a time. If the bit is 1 then we send the 8 bits of row out. Otherwise we send 0. 
// We send onBits at the first phase of the signal generation. We could just send 0xff, but that mught enable pull-ups on pins that we are not using. 

/// Unforntunately we have to drop to ASM for this so we can interleave the computaions durring the delays, otherwise things get too slow.


// NOTE THAT THE ORDER OF THE PARAMETERS IS IMPORTANT! WE DEPEND ON THESE IN SEDNROWRGB!

static inline void sendRow(  const uint8_t row , const uint8_t onBits ,const uint8_t colorbyte  ) {  
              
    asm volatile (


      "L_%=: \n\r"  
      
      "out %[port], %[onBits] \n\t"                // (1 cycles) - send T0H high (all bits high)
      
      "mov r0, %[bitwalker] \n\t"                  // (1 cycles) 
      "and r0, %[colorbyte] \n\t"                  // (1 cycles) - is the current bit in the color byte set?
      "mov r0, %[row]       \n\t"                  // (1 cycles) - get possible output byte ready (does not update Z)
      "brne ON_%= \n\t"                            // (1 cycles) - if zero after the and, then send full zero row
      "mov r0,r1  \n\r"                            // (1 cycles) - bit in colorbyte was zero, so send all 0's. 
      "ON_%=: \n\r"                                //              Note that if we land here becuase of brne, it takes 2 cycles, but it still takes 2 if the brne fell though to the mov
      
      // No extra delay here since the above calculation takes 7 cycles, using up the T0H of 350ns (https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=(350ns)%2F(1%2F(16mhz)))
            
      "out %[port], r0 \n\t"                       // (cycles 1) - set the output bits to [row] or 0x00 based on the bit in colorbyte. This is phase for T0H-T1H

      // We get T1H-T0H here, which is 350ns (6 cycles at 16mhz)

      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag

      "nop \n\t nop \n\t nop \n\t "
            
      "out %[port],__zero_reg__  \n\t"             // (1 cycles) last step - T1L all bits low
      
      "brcs DONE_%= \n\t"                          // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will tak long enough to cover the phase 3 delay
      
//      "DELAYC_%= %[offCycles]-5 \n\t"                 // Execute NOPs to delay exactly the specified number of cycles
      
      "jmp L_%= \n\t"                              // (3 cycles) 
            
      "DONE_%=: \n\t"

      // Don't need an explicit delay here since the overhead that follows will always be long enough
    
      ::
      [port]    "I" (_SFR_IO_ADDR(PIXEL_PORT)),
      [row]   "d" (row),
      [onBits]   "d" (onBits),
      [colorbyte]   "d" (colorbyte ),     // Phase 2 of the signal where the actual data bits show up.                
      [bitwalker] "r" (0x80)                      // Alocate a register to hold a bit that we will walk down though the color byte

    );
                                  

    // Note that the inter-bit gap can be as long as you want as long as it doesn't exceed the 5us reset timeout (which is A long time)
    // Here I have been generous and not tried to squeeze the gap tight but instead erred on the side of lots of extra time.
    // This has thenice side effect of avoid glitches on very long strings becuase 
    
}  


// Send one full pixel of color data - 3 bytes per pixel (24 bits in all)
// This is an ugly hack that resuses the paramaters when calling sendRow. It saves a bunch of PUSH/PULL operations per call.
// It would be cleaner to do this all in ASM, but I couldn't get inline marcos to work right, and the Arduino IDE won't let me add a .S file as a tab. Arg.

// Remeber that ws2812b take colors in order G,R,B

// NOTE THAT THE ORDER OF THE PARAMETERS IS IMPORTANT! WE DEPEND ON THESE IN SEDNROW!

static inline void sendRowRGB( const uint8_t row , const uint8_t onBits , const uint8_t red,  const uint8_t green,  const uint8_t blue  ) {

   asm volatile (

      "G_%=: \n\r"  
      
      "out %[port], %[onBits] \n\t"                // (1 cycles) - send T0H high (all bits high)
      
      "mov r0, %[bitwalker] \n\t"                  // (1 cycles) 
      "and r0, %[green] \n\t"                      // (1 cycles) - is the current bit in the color byte set?
      "mov r0, %[row]       \n\t"                  // (1 cycles) - get possible output byte ready (does not update Z)
      "brne ONG_%= \n\t"                            // (1 cycles) - if zero after the and, then send full zero row
      "mov r0,r1  \n\r"                            // (1 cycles) - bit in colorbyte was zero, so send all 0's. 
      "ONG_%=: \n\r"                                //              Note that if we land here becuase of brne, it takes 2 cycles, but it still takes 2 if the brne fell though to the mov
      
      // No extra delay here since the above calculation takes 7 cycles, using up the T0H of 350ns (https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=(350ns)%2F(1%2F(16mhz)))
            
      "out %[port], r0 \n\t"                       // (cycles 1) - set the output bits to [row] or 0x00 based on the bit in colorbyte. This is phase for T0H-T1H

      // We get T1H-T0H here, which is 350ns (6 cycles at 16mhz)

      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag

      "nop \n\t nop \n\t nop \n\t nop \n\t "
            
      "out %[port],__zero_reg__  \n\t"             // (1 cycles) last step - T1L all bits low

      "nop \n\t nop \n\t nop \n\t nop \n\t nop \n\t"
            
      "brcc G_%= \n\t"                              // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will tak long enough to cover the phase 3 delay
            

      // -------------------------------------------------------------


      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next cycle - moves the 1 in C to the top - and wastes one cycle that would have been used for the branch not taken above
      
      "R_%=: \n\r"  

      "out %[port], %[onBits] \n\t"                // (1 cycles) - send T0H high (all bits high)
      
      "mov r0, %[bitwalker] \n\t"                  // (1 cycles) 
      "and r0, %[red] \n\t"                  // (1 cycles) - is the current bit in the color byte set?
      "mov r0, %[row]       \n\t"                  // (1 cycles) - get possible output byte ready (does not update Z)
      "brne ONR_%= \n\t"                            // (1 cycles) - if zero after the and, then send full zero row
      "mov r0,r1  \n\r"                            // (1 cycles) - bit in colorbyte was zero, so send all 0's. 
      "ONR_%=: \n\r"                                //              Note that if we land here becuase of brne, it takes 2 cycles, but it still takes 2 if the brne fell though to the mov
      
      // No extra delay here since the above calculation takes 7 cycles, using up the T0H of 350ns (https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=(350ns)%2F(1%2F(16mhz)))
            
      "out %[port], r0 \n\t"                       // (cycles 1) - set the output bits to [row] or 0x00 based on the bit in colorbyte. This is phase for T0H-T1H

      // We get T1H-T0H here, which is 350ns (6 cycles at 16mhz)

      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag

      "nop \n\t nop \n\t nop \n\t nop \n\t "
            
      "out %[port],__zero_reg__  \n\t"             // (1 cycles) last step - T1L all bits low

      "nop \n\t nop \n\t nop \n\t nop \n\t nop \n\t"
      
      "brcc R_%= \n\t"                              // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will tak long enough to cover the phase 3 delay
                        

      // -------------------------------------------------------------
      
      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next cycle - moves the 1 in C to the top - and wastes one cycle that would have been used for the branch not taken above
      
      "B_%=: \n\r"  
      
      "out %[port], %[onBits] \n\t"                // (1 cycles) - send T0H high (all bits high)
      
      "mov r0, %[bitwalker] \n\t"                  // (1 cycles) 
      "and r0, %[blue] \n\t"                  // (1 cycles) - is the current bit in the color byte set?
      "mov r0, %[row]       \n\t"                  // (1 cycles) - get possible output byte ready (does not update Z)
      "brne ONB_%= \n\t"                            // (1 cycles) - if zero after the and, then send full zero row
      "mov r0,r1  \n\r"                            // (1 cycles) - bit in colorbyte was zero, so send all 0's. 
      "ONB_%=: \n\r"                                //              Note that if we land here becuase of brne, it takes 2 cycles, but it still takes 2 if the brne fell though to the mov
      
      // No extra delay here since the above calculation takes 7 cycles, using up the T0H of 350ns (https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=(350ns)%2F(1%2F(16mhz)))
            
      "out %[port], r0 \n\t"                       // (cycles 1) - set the output bits to [row] or 0x00 based on the bit in colorbyte. This is phase for T0H-T1H

      // We get T1H-T0H here, which is 350ns (6 cycles at 16mhz)

      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag

      "nop \n\t nop \n\t nop \n\t nop \n\t "
            
      "out %[port],__zero_reg__  \n\t"             // (1 cycles) last step - T1L all bits low

      // We do it a little differently on the last byte. We don't waste any time on the last bit to give ourselves the maximum ammmount of time to process between pixels
      
      "brcs DONE_%= \n\t"                              // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will tak long enough to cover the phase 3 delay
      
      "nop \n\t nop \n\t nop \n\t nop \n\t"       // We need one less nop here becuase the jmp takes 2 cycles
            
      "jmp B_%= \n\t"                              // (3 cycles) 
            
      // Note that we do not normalize the bitwalker here - it will get reloaded on the next call. One wasted cycle per pixel column not so bad.

      "DONE_%=: \n\r"  


      // Don't need an explicit delay here since the overhead that follows will always be long enough
    
      ::
      [port]    "I" (_SFR_IO_ADDR(PIXEL_PORT)),
      [row]   "d" (row),
      [onBits]   "d" (onBits),
      [red]   "d" (red),          // Phase 2 of the signal where the actual data bits show up.                
      [green]   "d" (green),      // Phase 2 of the signal where the actual data bits show up.                
      [blue]   "d" (blue),        // Phase 2 of the signal where the actual data bits show up.                
      [bitwalker] "r" (0x80)      // Alocate a register to hold a bit that we will walk down though the color byte

    );
                                  

    // Note that the inter-bit gap can be as long as you want as long as it doesn't exceed the 5us reset timeout (which is A long time)
    // Here I have been generous and not tried to squeeze the gap tight but instead erred on the side of lots of extra time.
    // This has thenice side effect of avoid glitches on very long strings becuase 

}

//  sendRowAsm( row , g , onBits);    // WS2812 takes colors in GRB order
//  sendRowAsm( row , r , onBits);    // WS2812 takes colors in GRB order
//  sendRowAsm( row , b , onBits);    // WS2812 takes colors in GRB order
  

// This nice 5x7 font from here...
// http://sunge.awardspace.com/glcd-sd/node4.html


// Font details:
// 1) Each char is fixed 5x7 pixels. 
// 2) Each byte is one column.
// 3) Columns are left to right order, leftmost byte is leftmost column of pixels.
// 4) Each column is 8 bits high.
// 5) Bit #7 is top line of char, Bit #1 is bottom.
// 6) Bit #0 is always 0, becuase this pin is used as serial input and setting to 1 would enable the pull-up.

// defines ascii characters 0x20-0x7F (32-127)
// PROGMEM after variable name as per https://www.arduino.cc/en/Reference/PROGMEM

#define CHAR_WIDTH 5      
#define INTERCHAR_SPACE 1
#define ASCII_OFFSET 0x20    // ASSCI code of 1st char in font array

const uint8_t Font5x7[] PROGMEM = {
0x00,0x00,0x00,0x00,0x00,//  
0x00,0x00,0xfa,0x00,0x00,// !
0x00,0xe0,0x00,0xe0,0x00,// "
0x28,0xfe,0x28,0xfe,0x28,// #
0x24,0x54,0xfe,0x54,0x48,// $
0xc4,0xc8,0x10,0x26,0x46,// %
0x6c,0x92,0xaa,0x44,0x0a,// &
0x00,0xa0,0xc0,0x00,0x00,// '
0x00,0x38,0x44,0x82,0x00,// (
0x00,0x82,0x44,0x38,0x00,// )
0x10,0x54,0x38,0x54,0x10,// *
0x10,0x10,0x7c,0x10,0x10,// +
0x00,0x0a,0x0c,0x00,0x00,// ,
0x10,0x10,0x10,0x10,0x10,// -
0x00,0x06,0x06,0x00,0x00,// .
0x04,0x08,0x10,0x20,0x40,// /
0x7c,0x8a,0x92,0xa2,0x7c,// 0
0x00,0x42,0xfe,0x02,0x00,// 1
0x42,0x86,0x8a,0x92,0x62,// 2
0x84,0x82,0xa2,0xd2,0x8c,// 3
0x18,0x28,0x48,0xfe,0x08,// 4
0xe4,0xa2,0xa2,0xa2,0x9c,// 5
0x3c,0x52,0x92,0x92,0x0c,// 6
0x80,0x8e,0x90,0xa0,0xc0,// 7
0x6c,0x92,0x92,0x92,0x6c,// 8
0x60,0x92,0x92,0x94,0x78,// 9
0x00,0x6c,0x6c,0x00,0x00,// :
0x00,0x6a,0x6c,0x00,0x00,// ;
0x00,0x10,0x28,0x44,0x82,// <
0x28,0x28,0x28,0x28,0x28,// =
0x82,0x44,0x28,0x10,0x00,// >
0x40,0x80,0x8a,0x90,0x60,// ?
0x4c,0x92,0x9e,0x82,0x7c,// @
0x7e,0x88,0x88,0x88,0x7e,// A
0xfe,0x92,0x92,0x92,0x6c,// B
0x7c,0x82,0x82,0x82,0x44,// C
0xfe,0x82,0x82,0x44,0x38,// D
0xfe,0x92,0x92,0x92,0x82,// E
0xfe,0x90,0x90,0x80,0x80,// F
0x7c,0x82,0x82,0x8a,0x4c,// G
0xfe,0x10,0x10,0x10,0xfe,// H
0x00,0x82,0xfe,0x82,0x00,// I
0x04,0x02,0x82,0xfc,0x80,// J
0xfe,0x10,0x28,0x44,0x82,// K
0xfe,0x02,0x02,0x02,0x02,// L
0xfe,0x40,0x20,0x40,0xfe,// M
0xfe,0x20,0x10,0x08,0xfe,// N
0x7c,0x82,0x82,0x82,0x7c,// O
0xfe,0x90,0x90,0x90,0x60,// P
0x7c,0x82,0x8a,0x84,0x7a,// Q
0xfe,0x90,0x98,0x94,0x62,// R
0x62,0x92,0x92,0x92,0x8c,// S
0x80,0x80,0xfe,0x80,0x80,// T
0xfc,0x02,0x02,0x02,0xfc,// U
0xf8,0x04,0x02,0x04,0xf8,// V
0xfe,0x04,0x18,0x04,0xfe,// W
0xc6,0x28,0x10,0x28,0xc6,// X
0xc0,0x20,0x1e,0x20,0xc0,// Y
0x86,0x8a,0x92,0xa2,0xc2,// Z
0x00,0x00,0xfe,0x82,0x82,// [
0x40,0x20,0x10,0x08,0x04,// (backslash)
0x82,0x82,0xfe,0x00,0x00,// ]
0x20,0x40,0x80,0x40,0x20,// ^
0x02,0x02,0x02,0x02,0x02,// _
0x00,0x80,0x40,0x20,0x00,// `
0x04,0x2a,0x2a,0x2a,0x1e,// a
0xfe,0x12,0x22,0x22,0x1c,// b
0x1c,0x22,0x22,0x22,0x04,// c
0x1c,0x22,0x22,0x12,0xfe,// d
0x1c,0x2a,0x2a,0x2a,0x18,// e
0x10,0x7e,0x90,0x80,0x40,// f
0x10,0x28,0x2a,0x2a,0x3c,// g
0xfe,0x10,0x20,0x20,0x1e,// h
0x00,0x22,0xbe,0x02,0x00,// i
0x04,0x02,0x22,0xbc,0x00,// j
0x00,0xfe,0x08,0x14,0x22,// k
0x00,0x82,0xfe,0x02,0x00,// l
0x3e,0x20,0x18,0x20,0x1e,// m
0x3e,0x10,0x20,0x20,0x1e,// n
0x1c,0x22,0x22,0x22,0x1c,// o
0x3e,0x28,0x28,0x28,0x10,// p
0x10,0x28,0x28,0x18,0x3e,// q
0x3e,0x10,0x20,0x20,0x10,// r
0x12,0x2a,0x2a,0x2a,0x04,// s
0x20,0xfc,0x22,0x02,0x04,// t
0x3c,0x02,0x02,0x04,0x3e,// u
0x38,0x04,0x02,0x04,0x38,// v
0x3c,0x02,0x0c,0x02,0x3c,// w
0x22,0x14,0x08,0x14,0x22,// x
0x30,0x0a,0x0a,0x0a,0x3c,// y
0x22,0x26,0x2a,0x32,0x22,// z
0x00,0x10,0x6c,0x82,0x00,// {
0x00,0x00,0xfe,0x00,0x00,// |
0x00,0x82,0x6c,0x10,0x00,// }
0x10,0x10,0x54,0x38,0x10,// ~
0x10,0x38,0x54,0x10,0x10,// 
};

// Send the pixels to form the specified char, not including intercase space
// skip is the number of pixels to skip at the begining to enable sub-char smooth scrolling

// TODO: Subtract the offset from the char before starting the send sequence to save time if nessisary
// TODO: Also could pad the begining of the font table to aovid the offset subtraction at the cost of 20*8 bytes of progmem

static inline void sendCharWithSkip( const uint8_t c ,  uint8_t skip , const uint8_t r,  const uint8_t g,  const uint8_t b , const uint8_t onBits) {

  const uint8_t *charbase = Font5x7 + (( c -' ')*5) ; 

  uint8_t col=CHAR_WIDTH; 

  while (skip--) {
      charbase++;
      col--;    
  }
  
  while (col--) {
      sendRowRGB( pgm_read_byte_near( charbase++ ) , onBits , r , g , b );
  }    

  // TODO: FLexible interchar spacing

  sendRowRGB( 0 , onBits , r , g , b );    // Interchar space
  
}

/*

static inline void sendChar( const uint8_t c ,  const uint8_t r,  const uint8_t g,  const uint8_t b , const uint8_t onBits) {

  const uint8_t *charbase = Font5x7 + (( c -' ')*5) ; 

  uint8_t col=CHAR_WIDTH;   
  while (col--) {
      sendRowRGB( pgm_read_byte_near( charbase++ ) , onBits , r , g , b );
  }    

  // TODO: FLexible interchar spacing

  sendRowRGB( 0 , onBits , r , g , b );    // Interchar space
  
}

*/

// Show the passed string. The last letter of the string will be in the rightmost pixels of the display.
// Skip is how many cols of the 1st char to skip for smooth scrolling

static inline void sendString( const char *s , uint8_t skip ,  const uint8_t r,  const uint8_t g,  const uint8_t b , const uint8_t onBits ) {

  unsigned int l=PIXELS/(CHAR_WIDTH+INTERCHAR_SPACE); 
  
  sendCharWithSkip( *s , skip ,  r , g , b, onBits );   // First char is special case becuase it can be stepped for smooth scrolling

  while ( *(++s) && l--) {

///uint8_t interchar = INTERCHAR_SPACE;

/* TODO: Variable interchar spacing
   while (interchar--) {
      sendRowRGB( 0 , r , g , b, onBits );   // Inter char spacing
   }
*/

    PORTB |= 0x01;    // TODO: Benchmark only
    sendCharWithSkip( *s , 0 , r , g , b, onBits );
    PORTB &= ~0x01;   // TODO: Benchmark only     

  }
}

/*

  The following three functions are the public API:
  
  ledSetup() - set up the pin that is connected to the string. Call once at the begining of the program.  
  sendPixel( r g , b ) - send a single pixel to the string. Call this once for each pixel in a frame.
  show() - show the recently sent pixel on the LEDs . Call once per frame. 
  
*/


// Set the specified pins up as digital out

void ledsetup() {

  PIXEL_DDR = 0xff; // TODO: FIX PIXEL_BITMASK;    // Set all used pins to output
  
  
}

// Just wait long enough without sending any bots to cause the pixels to latch and display the last sent frame

void show() {
  delayMicroseconds( (RES / 1000UL) + 1);       // Round up since the delay must be _at_least_ this long (too short might not work, too long not a problem)
}


/*

  That is the whole API. What follows are some demo functions rewriten from the AdaFruit strandtest code...
  
  https://github.com/adafruit/Adafruit_NeoPixel/blob/master/examples/strandtest/strandtest.ino
  
  Note that we always turn off interrupts while we are sending pixels becuase an interupt
  could happen just when we were in the middle of somehting time sensitive.
  
  If we wanted to minimize the time interrupts were off, we could instead 
  could get away with only turning off interrupts just for the very brief moment 
  when we are actually sending a 0 bit (~1us), as long as we were sure that the total time 
  taken by any interrupts + the time in our pixel generation code never exceeded the reset time (5us).
  
*/


void setup() {
    
  ledsetup();  
  delay(100);
  
}


void loop() {

/*  
  DDRB = 0x01;
  PORTB |= 0x01;    // TODO: Benchmark only
  sendRowRGB( 0b00000010 , 0xfe , 0b10000000, 0b10000000, 0x00 );
  sendRowRGB( 0b10101010 , 0xfe , 0x00 , 0xff, 0x00 );
  PORTB &= ~0x01;    // TODO: Benchmark only
  
  delay(100);

 return;

*/

  const uint8_t onBits = PIXEL_BITMASK;    // Bits that we don't control in the PORT that are already ON, so we can preserve thier state

  // TODO: Actually sample the state of the pullup on unused pins and OR it into the mask so we maintain the state.
  // Must do AFTER the cli(). 
  // TODO: Add offBits also to maintain the pullup state of unused pins. 
  
  const char *m =   
          
      "Twas brillig, and the slithy toves "
            "Did gyre and gimble in the wabe: "
      "All mimsy were the borogoves, "
            "And the mome raths outgrabe. "
    
      "Beware the Jabberwock, my son! "
            "The jaws that bite, the claws that catch! "
      "Beware the Jubjub bird, and shun "      
            "The frumious Bandersnatch! "
      
      "He took his vorpal sword in hand; "
            "Long time the manxome foe he sought— "
      "So rested he by the Tumtum tree "
            "And stood awhile in thought. "
      
      "And, as in uffish thought he stood, "
            "The Jabberwock, with eyes of flame, "
      "Came whiffling through the tulgey wood, "      
            "And burbled as it came! "
      
      "One, two! One, two! And through and through "
            "The vorpal blade went snicker-snack! "
      "He left it dead, and with its head "
            "He went galumphing back. "
      
      "And hast thou slain the Jabberwock? "
            "Come to my arms, my beamish boy! "
      "O frabjous day! Callooh! Callay! "
            "He chortled in his joy. "
      
      "Twas brillig, and the slithy toves "
            "Did gyre and gimble in the wabe: "
      "All mimsy were the borogoves, "
            "And the mome raths outgrabe."  

      ;

  int colorcycle=0;

  DDRB |= 0x01;

  while (*m) {      

      colorcycle++;

      if (colorcycle>=256*3) {
        colorcycle=0;
      }

      uint8_t r,g,b;

      if (colorcycle<256) {
        r=colorcycle;
        b=255-colorcycle;
      } else if (colorcycle<512) {
        g=colorcycle;
        r=255-colorcycle;
      } else {
        b=colorcycle;
        g=255-colorcycle;
      }


    
    for( uint8_t step=0; step<CHAR_WIDTH+INTERCHAR_SPACE  ; step++ ) {   // step though each column of the 1st char for smooth scrolling



      cli();

      sendString( m , step ,r, g ,  b , onBits );
      

      sei();

      
      delay(1);

    }

    m++;

  }

  delay(1000);
  return;  
}



