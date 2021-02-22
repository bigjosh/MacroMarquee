// This simplified demo scrolls the text of the Jaberwoky poem directly from flash memory
// Full article at  http://wp.josh.com/2016/05/20/huge-scrolling-arduino-led-sign/

// Change this to be at least as long as your pixel string (too long will work fine, just be a little slower)

#define PIXELS 96*4  // Number of pixels in the string. I am using 4 meters of 96LED/M

// These values depend on which pins your 8 strings are connected to and what board you are using 
// More info on how to find these at http://www.arduino.cc/en/Reference/PortManipulation

// PORTD controls Digital Pins 0-7 on the Uno

// You'll need to look up the port/bit combination for other boards. 

// Note that you could also include the DigitalWriteFast header file to not need to to this lookup.

#define PIXEL_PORT  PORTD  // Port of the pin the pixels are connected to
#define PIXEL_DDR   DDRD   // Port of the pin the pixels are connected to


static const uint8_t onBits=0b11111110;   // Bit pattern to write to port to turn on all pins connected to LED strips. 
                                          // If you do not want to use all 8 pins, you can mask off the ones you don't want
                                          // Note that these will still get 0 written to them when we send pixels
                                          // TODO: If we have time, we could even add a variable that will and/or into the bits before writing to the port to support any combination of bits/values                                  

// These are the timing constraints taken mostly from 
// imperically measuring the output from the Adafruit library strandtest program

// Note that some of these defined values are for refernce only - the actual timing is determinted by the hard code.

#define T1H  814    // Width of a 1 bit in ns - 13 cycles
#define T1L  438    // Width of a 1 bit in ns -  7 cycles

#define T0H  312    // Width of a 0 bit in ns -  5 cycles
#define T0L  936    // Width of a 0 bit in ns - 15 cycles 

// Phase #1 - Always 1  - 5 cycles
// Phase #2 - Data part - 8 cycles
// Phase #3 - Always 0  - 7 cycles


#define RES 500000   // Width of the low gap between bits to cause a frame to latch

// Here are some convience defines for using nanoseconds specs to generate actual CPU delays

#define NS_PER_SEC (1000000000L)          // Note that this has to be SIGNED since we want to be able to check for negative values of derivatives

#define CYCLES_PER_SEC (F_CPU)

#define NS_PER_CYCLE ( NS_PER_SEC / CYCLES_PER_SEC )

#define NS_TO_CYCLES(n) ( (n) / NS_PER_CYCLE )


// Sends a full 8 bits down all the pins, represening a single color of 1 pixel
// We walk though the 8 bits in colorbyte one at a time. If the bit is 1 then we send the 8 bits of row out. Otherwise we send 0. 
// We send onBits at the first phase of the signal generation. We could just send 0xff, but that mught enable pull-ups on pins that we are not using. 

/// Unforntunately we have to drop to ASM for this so we can interleave the computaions durring the delays, otherwise things get too slow.

// OnBits is the mask of which bits are connected to strips. We pass it on so that we
// do not turn on unused pins becuase this would enable the pullup. Also, hopefully passing this
// will cause the compiler to allocate a Register for it and avoid a reload every pass.

static inline void sendBitx8(  const uint8_t row , const uint8_t colorbyte , const uint8_t onBits ) {  
              
    asm volatile (


      "L_%=: \n\r"  
      
      "out %[port], %[onBits] \n\t"                 // (1 cycles) - send either T0H or the first part of T1H. Onbits is a mask of which bits have strings attached.

      // Next determine if we are going to be sending 1s or 0s based on the current bit in the color....
      
      "mov r0, %[bitwalker] \n\t"                   // (1 cycles) 
      "and r0, %[colorbyte] \n\t"                   // (1 cycles)  - is the current bit in the color byte set?
      "breq OFF_%= \n\t"                            // (1 cycles) - bit in color is 0, then send full zero row (takes 2 cycles if branch taken, count the extra 1 on the target line)

      // If we get here, then we want to send a 1 for every row that has an ON dot...
      "nop \n\t  "                                  // (1 cycles) 
      "out %[port], %[row]   \n\t"                  // (1 cycles) - set the output bits to [row] This is phase for T0H-T1H.
                                                    // ==========
                                                    // (5 cycles) - T0H (Phase #1)


      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t "                                   // (1 cycles) 

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (8 cycles) - Phase #2
                                                    
      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag
                  
      "brcs DONE_%= \n\t"                          // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will tak long enough to cover the phase 3 delay

      "nop \n\t \n\t "                             // (1 cycles) - When added to the 5 cycles in S:, we gte the 7 cycles of T1L
            
      "jmp L_%= \n\t"                              // (3 cycles) 
                                                   // (1 cycles) - The OUT on the next pass of the loop
                                                   // ==========
                                                   // (7 cycles) - T1L
                                                   
                                                          
      "OFF_%=: \n\r"                                // (1 cycles)    Note that we land here becuase of breq, which takes takes 2 cycles

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (5 cycles) - T0H

      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag
                  
      "brcs DONE_%= \n\t"                          // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will tak long enough to cover the phase 3 delay

      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles)             
      "nop \n\t nop \n\t "                          // (2 cycles)             
      "nop \n\t "                                   // (1 cycles)             
            
      "jmp L_%= \n\t"                               // (3 cycles) 
                                                    // (1 cycles) - The OUT on the next pass of the loop      
                                                    // ==========
                                                    //(15 cycles) - T0L 
      
            
      "DONE_%=: \n\t"

      // Don't need an explicit delay here since the overhead that follows will always be long enough
    
      ::
      [port]    "I" (_SFR_IO_ADDR(PIXEL_PORT)),
      [row]   "d" (row),
      [onBits]   "d" (onBits),
      [colorbyte]   "d" (colorbyte ),     // Phase 2 of the signal where the actual data bits show up.                
      [bitwalker] "r" (0x80)                      // Alocate a register to hold a bit that we will walk down though the color byte

    );
                                  
    // Note that the inter-bit gap can be as long as you want as long as it doesn't exceed the reset timeout (which is A long time)
    
} 




// Just wait long enough without sending any bots to cause the pixels to latch and display the last sent frame

void show() {
  delayMicroseconds( (RES / 1000UL) + 1);       // Round up since the delay must be _at_least_ this long (too short might not work, too long not a problem)
}


// Send 3 bytes of color data (R,G,B) for a signle pixel down all the connected stringsat the same time
// A 1 bit in "row" means send the color, a 0 bit means send black. 

static inline void sendRowRGB( uint8_t row ,  uint8_t r,  uint8_t g,  uint8_t b ) {

  sendBitx8( row , g , onBits);    // WS2812 takes colors in GRB order
  sendBitx8( row , r , onBits);    // WS2812 takes colors in GRB order
  sendBitx8( row , b , onBits);    // WS2812 takes colors in GRB order
  
}

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

#define FONT_WIDTH 5      
#define INTERCHAR_SPACE 1
#define ASCII_OFFSET 0x20    // ASSCI code of 1st char in font array

using letter_record_t = struct  {
    uint8_t c;
    uint8_t width;
};

using fontline_t = union  {
  letter_record_t letter_record;
  uint16_t colbits;
};

constexpr fontline_t LETTER(uint8_t c,uint8_t len)  { return fontline_t { .letter_record = { c , len} } ; }

constexpr fontline_t BITS(uint16_t x) { return fontline_t { .colbits = x } ; } 

const fontline_t font[] PROGMEM = {
      
    LETTER( ' ' , 5 ),  // ASCII 0x20
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( '!' , 5 ),  // ASCII 0x21
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),
      BITS( 0b11111010 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( '\"' , 5 ),  // ASCII 0x22
      BITS( 0b00000000 ),
      BITS( 0b11100000 ),
      BITS( 0b00000000 ),
      BITS( 0b11100000 ),
      BITS( 0b00000000 ),


    LETTER( '#' , 5 ),  // ASCII 0x23
      BITS( 0b00101000 ),
      BITS( 0b11111110 ),
      BITS( 0b00101000 ),
      BITS( 0b11111110 ),
      BITS( 0b00101000 ),


    LETTER( '$' , 5 ),  // ASCII 0x24
      BITS( 0b00100100 ),
      BITS( 0b01010100 ),
      BITS( 0b11111110 ),
      BITS( 0b01010100 ),
      BITS( 0b01001000 ),


    LETTER( '%' , 5 ),  // ASCII 0x25
      BITS( 0b11000100 ),
      BITS( 0b11001000 ),
      BITS( 0b00010000 ),
      BITS( 0b00100110 ),
      BITS( 0b01000110 ),


    LETTER( '&' , 5 ),  // ASCII 0x26
      BITS( 0b01101100 ),
      BITS( 0b10010010 ),
      BITS( 0b10101010 ),
      BITS( 0b01000100 ),
      BITS( 0b00001010 ),


    LETTER( '\'' , 5 ),  // ASCII 0x27
      BITS( 0b00000000 ),
      BITS( 0b10100000 ),
      BITS( 0b11000000 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( '(' , 5 ),  // ASCII 0x28
      BITS( 0b00000000 ),
      BITS( 0b00111000 ),
      BITS( 0b01000100 ),
      BITS( 0b10000010 ),
      BITS( 0b00000000 ),


    LETTER( ')' , 5 ),  // ASCII 0x29
      BITS( 0b00000000 ),
      BITS( 0b10000010 ),
      BITS( 0b01000100 ),
      BITS( 0b00111000 ),
      BITS( 0b00000000 ),


    LETTER( '*' , 5 ),  // ASCII 0x2a
      BITS( 0b00010000 ),
      BITS( 0b01010100 ),
      BITS( 0b00111000 ),
      BITS( 0b01010100 ),
      BITS( 0b00010000 ),


    LETTER( '+' , 5 ),  // ASCII 0x2b
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),
      BITS( 0b01111100 ),
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),


    LETTER( ',' , 5 ),  // ASCII 0x2c
      BITS( 0b00000000 ),
      BITS( 0b00001010 ),
      BITS( 0b00001100 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( '-' , 5 ),  // ASCII 0x2d
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),


    LETTER( '.' , 5 ),  // ASCII 0x2e
      BITS( 0b00000000 ),
      BITS( 0b00000110 ),
      BITS( 0b00000110 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( '/' , 5 ),  // ASCII 0x2f
      BITS( 0b00000100 ),
      BITS( 0b00001000 ),
      BITS( 0b00010000 ),
      BITS( 0b00100000 ),
      BITS( 0b01000000 ),


    LETTER( '0' , 5 ),  // ASCII 0x30
      BITS( 0b01111100 ),
      BITS( 0b10001010 ),
      BITS( 0b10010010 ),
      BITS( 0b10100010 ),
      BITS( 0b01111100 ),


    LETTER( '1' , 5 ),  // ASCII 0x31
      BITS( 0b00000000 ),
      BITS( 0b01000010 ),
      BITS( 0b11111110 ),
      BITS( 0b00000010 ),
      BITS( 0b00000000 ),


    LETTER( '2' , 5 ),  // ASCII 0x32
      BITS( 0b01000010 ),
      BITS( 0b10000110 ),
      BITS( 0b10001010 ),
      BITS( 0b10010010 ),
      BITS( 0b01100010 ),


    LETTER( '3' , 5 ),  // ASCII 0x33
      BITS( 0b10000100 ),
      BITS( 0b10000010 ),
      BITS( 0b10100010 ),
      BITS( 0b11010010 ),
      BITS( 0b10001100 ),


    LETTER( '4' , 5 ),  // ASCII 0x34
      BITS( 0b00011000 ),
      BITS( 0b00101000 ),
      BITS( 0b01001000 ),
      BITS( 0b11111110 ),
      BITS( 0b00001000 ),


    LETTER( '5' , 5 ),  // ASCII 0x35
      BITS( 0b11100100 ),
      BITS( 0b10100010 ),
      BITS( 0b10100010 ),
      BITS( 0b10100010 ),
      BITS( 0b10011100 ),


    LETTER( '6' , 5 ),  // ASCII 0x36
      BITS( 0b00111100 ),
      BITS( 0b01010010 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b00001100 ),


    LETTER( '7' , 5 ),  // ASCII 0x37
      BITS( 0b10000000 ),
      BITS( 0b10001110 ),
      BITS( 0b10010000 ),
      BITS( 0b10100000 ),
      BITS( 0b11000000 ),


    LETTER( '8' , 5 ),  // ASCII 0x38
      BITS( 0b01101100 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b01101100 ),


    LETTER( '9' , 5 ),  // ASCII 0x39
      BITS( 0b01100000 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b10010100 ),
      BITS( 0b01111000 ),


    LETTER( ':' , 5 ),  // ASCII 0x3a
      BITS( 0b00000000 ),
      BITS( 0b01101100 ),
      BITS( 0b01101100 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( ';' , 5 ),  // ASCII 0x3b
      BITS( 0b00000000 ),
      BITS( 0b01101010 ),
      BITS( 0b01101100 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( '<' , 5 ),  // ASCII 0x3c
      BITS( 0b00000000 ),
      BITS( 0b00010000 ),
      BITS( 0b00101000 ),
      BITS( 0b01000100 ),
      BITS( 0b10000010 ),


    LETTER( '=' , 5 ),  // ASCII 0x3d
      BITS( 0b00101000 ),
      BITS( 0b00101000 ),
      BITS( 0b00101000 ),
      BITS( 0b00101000 ),
      BITS( 0b00101000 ),


    LETTER( '>' , 5 ),  // ASCII 0x3e
      BITS( 0b10000010 ),
      BITS( 0b01000100 ),
      BITS( 0b00101000 ),
      BITS( 0b00010000 ),
      BITS( 0b00000000 ),


    LETTER( '?' , 5 ),  // ASCII 0x3f
      BITS( 0b01000000 ),
      BITS( 0b10000000 ),
      BITS( 0b10001010 ),
      BITS( 0b10010000 ),
      BITS( 0b01100000 ),


    LETTER( '@' , 5 ),  // ASCII 0x40
      BITS( 0b01001100 ),
      BITS( 0b10010010 ),
      BITS( 0b10011110 ),
      BITS( 0b10000010 ),
      BITS( 0b01111100 ),


    LETTER( 'A' , 5 ),  // ASCII 0x41
      BITS( 0b01111110 ),
      BITS( 0b10001000 ),
      BITS( 0b10001000 ),
      BITS( 0b10001000 ),
      BITS( 0b01111110 ),


    LETTER( 'B' , 5 ),  // ASCII 0x42
      BITS( 0b11111110 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b01101100 ),


    LETTER( 'C' , 5 ),  // ASCII 0x43
      BITS( 0b01111100 ),
      BITS( 0b10000010 ),
      BITS( 0b10000010 ),
      BITS( 0b10000010 ),
      BITS( 0b01000100 ),


    LETTER( 'D' , 5 ),  // ASCII 0x44
      BITS( 0b11111110 ),
      BITS( 0b10000010 ),
      BITS( 0b10000010 ),
      BITS( 0b01000100 ),
      BITS( 0b00111000 ),


    LETTER( 'E' , 5 ),  // ASCII 0x45
      BITS( 0b11111110 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b10000010 ),


    LETTER( 'F' , 5 ),  // ASCII 0x46
      BITS( 0b11111110 ),
      BITS( 0b10010000 ),
      BITS( 0b10010000 ),
      BITS( 0b10000000 ),
      BITS( 0b10000000 ),


    LETTER( 'G' , 5 ),  // ASCII 0x47
      BITS( 0b01111100 ),
      BITS( 0b10000010 ),
      BITS( 0b10000010 ),
      BITS( 0b10001010 ),
      BITS( 0b01001100 ),


    LETTER( 'H' , 5 ),  // ASCII 0x48
      BITS( 0b11111110 ),
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),
      BITS( 0b11111110 ),


    LETTER( 'I' , 5 ),  // ASCII 0x49
      BITS( 0b00000000 ),
      BITS( 0b10000010 ),
      BITS( 0b11111110 ),
      BITS( 0b10000010 ),
      BITS( 0b00000000 ),


    LETTER( 'J' , 5 ),  // ASCII 0x4a
      BITS( 0b00000100 ),
      BITS( 0b00000010 ),
      BITS( 0b10000010 ),
      BITS( 0b11111100 ),
      BITS( 0b10000000 ),


    LETTER( 'K' , 5 ),  // ASCII 0x4b
      BITS( 0b11111110 ),
      BITS( 0b00010000 ),
      BITS( 0b00101000 ),
      BITS( 0b01000100 ),
      BITS( 0b10000010 ),


    LETTER( 'L' , 5 ),  // ASCII 0x4c
      BITS( 0b11111110 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),


    LETTER( 'M' , 5 ),  // ASCII 0x4d
      BITS( 0b11111110 ),
      BITS( 0b01000000 ),
      BITS( 0b00100000 ),
      BITS( 0b01000000 ),
      BITS( 0b11111110 ),


    LETTER( 'N' , 5 ),  // ASCII 0x4e
      BITS( 0b11111110 ),
      BITS( 0b00100000 ),
      BITS( 0b00010000 ),
      BITS( 0b00001000 ),
      BITS( 0b11111110 ),


    LETTER( 'O' , 5 ),  // ASCII 0x4f
      BITS( 0b01111100 ),
      BITS( 0b10000010 ),
      BITS( 0b10000010 ),
      BITS( 0b10000010 ),
      BITS( 0b01111100 ),


    LETTER( 'P' , 5 ),  // ASCII 0x50
      BITS( 0b11111110 ),
      BITS( 0b10010000 ),
      BITS( 0b10010000 ),
      BITS( 0b10010000 ),
      BITS( 0b01100000 ),


    LETTER( 'Q' , 5 ),  // ASCII 0x51
      BITS( 0b01111100 ),
      BITS( 0b10000010 ),
      BITS( 0b10001010 ),
      BITS( 0b10000100 ),
      BITS( 0b01111010 ),


    LETTER( 'R' , 5 ),  // ASCII 0x52
      BITS( 0b11111110 ),
      BITS( 0b10010000 ),
      BITS( 0b10011000 ),
      BITS( 0b10010100 ),
      BITS( 0b01100010 ),


    LETTER( 'S' , 5 ),  // ASCII 0x53
      BITS( 0b01100010 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b10010010 ),
      BITS( 0b10001100 ),


    LETTER( 'T' , 5 ),  // ASCII 0x54
      BITS( 0b10000000 ),
      BITS( 0b10000000 ),
      BITS( 0b11111110 ),
      BITS( 0b10000000 ),
      BITS( 0b10000000 ),


    LETTER( 'U' , 5 ),  // ASCII 0x55
      BITS( 0b11111100 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),
      BITS( 0b11111100 ),


    LETTER( 'V' , 5 ),  // ASCII 0x56
      BITS( 0b11111000 ),
      BITS( 0b00000100 ),
      BITS( 0b00000010 ),
      BITS( 0b00000100 ),
      BITS( 0b11111000 ),


    LETTER( 'W' , 5 ),  // ASCII 0x57
      BITS( 0b11111110 ),
      BITS( 0b00000100 ),
      BITS( 0b00011000 ),
      BITS( 0b00000100 ),
      BITS( 0b11111110 ),


    LETTER( 'X' , 5 ),  // ASCII 0x58
      BITS( 0b11000110 ),
      BITS( 0b00101000 ),
      BITS( 0b00010000 ),
      BITS( 0b00101000 ),
      BITS( 0b11000110 ),


    LETTER( 'Y' , 5 ),  // ASCII 0x59
      BITS( 0b11000000 ),
      BITS( 0b00100000 ),
      BITS( 0b00011110 ),
      BITS( 0b00100000 ),
      BITS( 0b11000000 ),


    LETTER( 'Z' , 5 ),  // ASCII 0x5a
      BITS( 0b10000110 ),
      BITS( 0b10001010 ),
      BITS( 0b10010010 ),
      BITS( 0b10100010 ),
      BITS( 0b11000010 ),


    LETTER( '[' , 5 ),  // ASCII 0x5b
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),
      BITS( 0b11111110 ),
      BITS( 0b10000010 ),
      BITS( 0b10000010 ),


    LETTER( '\\' , 5 ),  // ASCII 0x5c
      BITS( 0b01000000 ),
      BITS( 0b00100000 ),
      BITS( 0b00010000 ),
      BITS( 0b00001000 ),
      BITS( 0b00000100 ),


    LETTER( ']' , 5 ),  // ASCII 0x5d
      BITS( 0b10000010 ),
      BITS( 0b10000010 ),
      BITS( 0b11111110 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( '^' , 5 ),  // ASCII 0x5e
      BITS( 0b00100000 ),
      BITS( 0b01000000 ),
      BITS( 0b10000000 ),
      BITS( 0b01000000 ),
      BITS( 0b00100000 ),


    LETTER( '_' , 5 ),  // ASCII 0x5f
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),


    LETTER( '`' , 5 ),  // ASCII 0x60
      BITS( 0b00000000 ),
      BITS( 0b10000000 ),
      BITS( 0b01000000 ),
      BITS( 0b00100000 ),
      BITS( 0b00000000 ),


    LETTER( 'a' , 5 ),  // ASCII 0x61
      BITS( 0b00000100 ),
      BITS( 0b00101010 ),
      BITS( 0b00101010 ),
      BITS( 0b00101010 ),
      BITS( 0b00011110 ),


    LETTER( 'b' , 5 ),  // ASCII 0x62
      BITS( 0b11111110 ),
      BITS( 0b00010010 ),
      BITS( 0b00100010 ),
      BITS( 0b00100010 ),
      BITS( 0b00011100 ),


    LETTER( 'c' , 5 ),  // ASCII 0x63
      BITS( 0b00011100 ),
      BITS( 0b00100010 ),
      BITS( 0b00100010 ),
      BITS( 0b00100010 ),
      BITS( 0b00000100 ),


    LETTER( 'd' , 5 ),  // ASCII 0x64
      BITS( 0b00011100 ),
      BITS( 0b00100010 ),
      BITS( 0b00100010 ),
      BITS( 0b00010010 ),
      BITS( 0b11111110 ),


    LETTER( 'e' , 5 ),  // ASCII 0x65
      BITS( 0b00011100 ),
      BITS( 0b00101010 ),
      BITS( 0b00101010 ),
      BITS( 0b00101010 ),
      BITS( 0b00011000 ),


    LETTER( 'f' , 5 ),  // ASCII 0x66
      BITS( 0b00010000 ),
      BITS( 0b01111110 ),
      BITS( 0b10010000 ),
      BITS( 0b10000000 ),
      BITS( 0b01000000 ),


    LETTER( 'g' , 5 ),  // ASCII 0x67
      BITS( 0b00010000 ),
      BITS( 0b00101000 ),
      BITS( 0b00101010 ),
      BITS( 0b00101010 ),
      BITS( 0b00111100 ),


    LETTER( 'h' , 5 ),  // ASCII 0x68
      BITS( 0b11111110 ),
      BITS( 0b00010000 ),
      BITS( 0b00100000 ),
      BITS( 0b00100000 ),
      BITS( 0b00011110 ),


    LETTER( 'i' , 5 ),  // ASCII 0x69
      BITS( 0b00000000 ),
      BITS( 0b00100010 ),
      BITS( 0b10111110 ),
      BITS( 0b00000010 ),
      BITS( 0b00000000 ),


    LETTER( 'j' , 5 ),  // ASCII 0x6a
      BITS( 0b00000100 ),
      BITS( 0b00000010 ),
      BITS( 0b00100010 ),
      BITS( 0b10111100 ),
      BITS( 0b00000000 ),


    LETTER( 'k' , 5 ),  // ASCII 0x6b
      BITS( 0b00000000 ),
      BITS( 0b11111110 ),
      BITS( 0b00001000 ),
      BITS( 0b00010100 ),
      BITS( 0b00100010 ),


    LETTER( 'l' , 5 ),  // ASCII 0x6c
      BITS( 0b00000000 ),
      BITS( 0b10000010 ),
      BITS( 0b11111110 ),
      BITS( 0b00000010 ),
      BITS( 0b00000000 ),


    LETTER( 'm' , 5 ),  // ASCII 0x6d
      BITS( 0b00111110 ),
      BITS( 0b00100000 ),
      BITS( 0b00011000 ),
      BITS( 0b00100000 ),
      BITS( 0b00011110 ),


    LETTER( 'n' , 5 ),  // ASCII 0x6e
      BITS( 0b00111110 ),
      BITS( 0b00010000 ),
      BITS( 0b00100000 ),
      BITS( 0b00100000 ),
      BITS( 0b00011110 ),


    LETTER( 'o' , 5 ),  // ASCII 0x6f
      BITS( 0b00011100 ),
      BITS( 0b00100010 ),
      BITS( 0b00100010 ),
      BITS( 0b00100010 ),
      BITS( 0b00011100 ),


    LETTER( 'p' , 5 ),  // ASCII 0x70
      BITS( 0b00111110 ),
      BITS( 0b00101000 ),
      BITS( 0b00101000 ),
      BITS( 0b00101000 ),
      BITS( 0b00010000 ),


    LETTER( 'q' , 5 ),  // ASCII 0x71
      BITS( 0b00010000 ),
      BITS( 0b00101000 ),
      BITS( 0b00101000 ),
      BITS( 0b00011000 ),
      BITS( 0b00111110 ),


    LETTER( 'r' , 5 ),  // ASCII 0x72
      BITS( 0b00111110 ),
      BITS( 0b00010000 ),
      BITS( 0b00100000 ),
      BITS( 0b00100000 ),
      BITS( 0b00010000 ),


    LETTER( 's' , 5 ),  // ASCII 0x73
      BITS( 0b00010010 ),
      BITS( 0b00101010 ),
      BITS( 0b00101010 ),
      BITS( 0b00101010 ),
      BITS( 0b00000100 ),


    LETTER( 't' , 5 ),  // ASCII 0x74
      BITS( 0b00100000 ),
      BITS( 0b11111100 ),
      BITS( 0b00100010 ),
      BITS( 0b00000010 ),
      BITS( 0b00000100 ),


    LETTER( 'u' , 5 ),  // ASCII 0x75
      BITS( 0b00111100 ),
      BITS( 0b00000010 ),
      BITS( 0b00000010 ),
      BITS( 0b00000100 ),
      BITS( 0b00111110 ),


    LETTER( 'v' , 5 ),  // ASCII 0x76
      BITS( 0b00111000 ),
      BITS( 0b00000100 ),
      BITS( 0b00000010 ),
      BITS( 0b00000100 ),
      BITS( 0b00111000 ),


    LETTER( 'w' , 5 ),  // ASCII 0x77
      BITS( 0b00111100 ),
      BITS( 0b00000010 ),
      BITS( 0b00001100 ),
      BITS( 0b00000010 ),
      BITS( 0b00111100 ),


    LETTER( 'x' , 5 ),  // ASCII 0x78
      BITS( 0b00100010 ),
      BITS( 0b00010100 ),
      BITS( 0b00001000 ),
      BITS( 0b00010100 ),
      BITS( 0b00100010 ),


    LETTER( 'y' , 5 ),  // ASCII 0x79
      BITS( 0b00110000 ),
      BITS( 0b00001010 ),
      BITS( 0b00001010 ),
      BITS( 0b00001010 ),
      BITS( 0b00111100 ),


    LETTER( 'z' , 5 ),  // ASCII 0x7a
      BITS( 0b00100010 ),
      BITS( 0b00100110 ),
      BITS( 0b00101010 ),
      BITS( 0b00110010 ),
      BITS( 0b00100010 ),


    LETTER( '{' , 5 ),  // ASCII 0x7b
      BITS( 0b00000000 ),
      BITS( 0b00010000 ),
      BITS( 0b01101100 ),
      BITS( 0b10000010 ),
      BITS( 0b00000000 ),


    LETTER( '|' , 5 ),  // ASCII 0x7c
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),
      BITS( 0b11111110 ),
      BITS( 0b00000000 ),
      BITS( 0b00000000 ),


    LETTER( '}' , 5 ),  // ASCII 0x7d
      BITS( 0b00000000 ),
      BITS( 0b10000010 ),
      BITS( 0b01101100 ),
      BITS( 0b00010000 ),
      BITS( 0b00000000 ),


    LETTER( '~' , 5 ),  // ASCII 0x7e
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),
      BITS( 0b01010100 ),
      BITS( 0b00111000 ),
      BITS( 0b00010000 ),


    LETTER( '' , 5 ),  // ASCII 0x7f
      BITS( 0b00010000 ),
      BITS( 0b00111000 ),
      BITS( 0b01010100 ),
      BITS( 0b00010000 ),
      BITS( 0b00010000 ),

};

// Send the pixels to form the specified char, not including interchar space
// skip is the number of pixels to skip at the begining to enable sub-char smooth scrolling

// TODO: Subtract the offset from the char before starting the send sequence to save time if nessisary
// TODO: Also could pad the begining of the font table to aovid the offset subtraction at the cost of 20*8 bytes of progmem
// TODO: Could pad all chars out to 8 bytes wide to turn the the multiply by FONT_WIDTH into a shift 

static inline void sendChar( uint8_t c ,  uint8_t skip , uint8_t r,  uint8_t g,  uint8_t b ) {

  const uint8_t *charbase = Font5x7 + (( c -' ')* FONT_WIDTH ) ; 

  uint8_t col=FONT_WIDTH; 

  while (skip--) {
      charbase++;
      col--;    
  }
  
  while (col--) {
      sendRowRGB( pgm_read_byte_near( charbase++ ) , r , g , b );
  }    
  
  col=INTERCHAR_SPACE;
  
  while (col--) {

    sendRowRGB( 0 , r , g , b );    // Interchar space
    
  }
  
}


// Show the passed string. The last letter of the string will be in the rightmost pixels of the display.
// Skip is how many cols of the 1st char to skip for smooth scrolling


static inline void sendString( const char *s , uint8_t skip ,  const uint8_t r,  const uint8_t g,  const uint8_t b ) {

  unsigned int l=PIXELS/(FONT_WIDTH+INTERCHAR_SPACE); 

  sendChar( *s , skip ,  r , g , b );   // First char is special case becuase it can be stepped for smooth scrolling
  
  while ( *(++s) && l--) {

    sendChar( *s , 0,  r , g , b );

  }

  show();
}

void setup() {
  PIXEL_DDR |= onBits;         // Set used pins to output mode
}

static char jabberText[] = 
      "                                       " 
      "Twas brillig, and the slithy toves "
            "Did gyre and gimble in the wabe: "
      "All mimsy were the borogoves, "
            "And the mome raths outgrabe. "
      
      "Beware the Jabberwock, my son! "
            "The jaws that bite, the claws that catch! "
      "Beware the Jubjub bird, and shun "      
            "The frumious Bandersnatch! "
      
      "He took his vorpal sword in hand; "
            "Long time the manxome foe he sought- "
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

void loop() {
  
  const char *m = jabberText;
              
  while (*m) {      


      for( uint8_t step=0; step<FONT_WIDTH+INTERCHAR_SPACE  ; step++ ) {   // step though each column of the 1st char for smooth scrolling

         cli();
  
         sendString( m , step , 0x00, 0x00 , 0x40 );    // Nice and not-too-bright blue hue
        
         sei();
         
  
      }

    m++;

  }


}
