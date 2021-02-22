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

      "mov r0, %[bitwalker] \n\t"                   //              Get ready to check if we should send all zeros
            
      "cli \n\t"                                   //              Turn off interrupts to make sure we meet the T0H deadline. 
      "out %[port], %[onBits] \n\t"                 //              Send either T0H or the first part of T1H. Onbits is a mask of which bits have strings attached.

      // Next determine if we are going to be sending 1s or 0s based on the current bit in the color....
      
      "and r0, %[colorbyte] \n\t"                   // (1 cycles)  - is the current bit in the color byte set?
      "breq OFF_%= \n\t"                            // (1 cycles) - bit in color is 0, then send full zero row (takes 2 cycles if branch taken, count the extra 1 on the target line)

      "nop \n\t \n\t "                             // (1 cycles) - Balances out the extra cycle on the other path
      
      // If we get here, then we want to send a 1 for every row that has an ON dot...
      // So if there is a 1 in [row] then the output will still high, otherwise it will go low
      // making a short zero bit signal. 
      "out %[port], %[row]   \n\t"                  // (1 cycles) - set the output bits to [row] This is phase for T0H-T1H.
                                                    // ==========
                                                    // (4 cycles) - T0H (Phase #1) 4 cycles / 16Mhz = 250 nanoseconds

      "sei \n\t"                                    // (1 cycles) OK to reenable interrupt now - T0H is the only timing sensitive phase https://wp.josh.com/2014/05/13/ws2812-neopixels-are-not-so-finicky-once-you-get-to-know-them/
                                                    
              
      "jmp NEXT_%= \n\t"                              // (3 cycles) 
                                                   // (1 cycles) - The OUT on the next pass of the loop
                                                   // ==========
                                                   // (7 cycles) - T1L
                                                   
                                                          
      "OFF_%=: \n\r"                                // (1 cycles)    Note that we land here becuase of breq, which takes takes 2 cycles

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (4 cycles) - T0H
                                                    
      "sei \n\t"                                    // (1 cycles) OK to reenable interrupt now - T0H is the only timing sensitive phase https://wp.josh.com/2014/05/13/ws2812-neopixels-are-not-so-finicky-once-you-get-to-know-them/

      "nop \n\t \n\t "                             // (1 cycles) - Balances out the extra cycle on the other path
                  
      "NEXT_%=: \n\t"

      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles) 

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (9 cycles) - T1H (Phase #2 ) 9 cycles / 16Mhz = 560ns      

      // OK we are done sending this set of bits. Now we need a bit of space for time between bits (T1L 600ns) 

      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles)      
      "nop \n\t nop \n\t "                          // (2 cycles)      

      "ror %[bitwalker] \n\t"                      // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag
                  
      "brcc L_%= \n\t"                             // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will takw long enough to cover the phase 3 delay

      
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

const uint8_t fontdata[][FONT_WIDTH] PROGMEM = {
    { //  ASCII 0x20 (' ')
        0b00000000 ,
        0b00000000 ,
        0b00000000 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x21 ('!')
        0b00000000 ,
        0b00000000 ,
        0b11111010 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x22 ('"')
        0b00000000 ,
        0b11100000 ,
        0b00000000 ,
        0b11100000 ,
        0b00000000 ,
    },

    { //  ASCII 0x23 ('#')
        0b00101000 ,
        0b11111110 ,
        0b00101000 ,
        0b11111110 ,
        0b00101000 ,
    },

    { //  ASCII 0x24 ('$')
        0b00100100 ,
        0b01010100 ,
        0b11111110 ,
        0b01010100 ,
        0b01001000 ,
    },

    { //  ASCII 0x25 ('%')
        0b11000100 ,
        0b11001000 ,
        0b00010000 ,
        0b00100110 ,
        0b01000110 ,
    },

    { //  ASCII 0x26 ('&')
        0b01101100 ,
        0b10010010 ,
        0b10101010 ,
        0b01000100 ,
        0b00001010 ,
    },

    { //  ASCII 0x27 (''')
        0b00000000 ,
        0b10100000 ,
        0b11000000 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x28 ('(')
        0b00000000 ,
        0b00111000 ,
        0b01000100 ,
        0b10000010 ,
        0b00000000 ,
    },

    { //  ASCII 0x29 (')')
        0b00000000 ,
        0b10000010 ,
        0b01000100 ,
        0b00111000 ,
        0b00000000 ,
    },

    { //  ASCII 0x2a ('*')
        0b00010000 ,
        0b01010100 ,
        0b00111000 ,
        0b01010100 ,
        0b00010000 ,
    },

    { //  ASCII 0x2b ('+')
        0b00010000 ,
        0b00010000 ,
        0b01111100 ,
        0b00010000 ,
        0b00010000 ,
    },

    { //  ASCII 0x2c (',')
        0b00000000 ,
        0b00001010 ,
        0b00001100 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x2d ('-')
        0b00010000 ,
        0b00010000 ,
        0b00010000 ,
        0b00010000 ,
        0b00010000 ,
    },

    { //  ASCII 0x2e ('.')
        0b00000000 ,
        0b00000110 ,
        0b00000110 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x2f ('/')
        0b00000100 ,
        0b00001000 ,
        0b00010000 ,
        0b00100000 ,
        0b01000000 ,
    },

    { //  ASCII 0x30 ('0')
        0b01111100 ,
        0b10001010 ,
        0b10010010 ,
        0b10100010 ,
        0b01111100 ,
    },

    { //  ASCII 0x31 ('1')
        0b00000000 ,
        0b01000010 ,
        0b11111110 ,
        0b00000010 ,
        0b00000000 ,
    },

    { //  ASCII 0x32 ('2')
        0b01000010 ,
        0b10000110 ,
        0b10001010 ,
        0b10010010 ,
        0b01100010 ,
    },

    { //  ASCII 0x33 ('3')
        0b10000100 ,
        0b10000010 ,
        0b10100010 ,
        0b11010010 ,
        0b10001100 ,
    },

    { //  ASCII 0x34 ('4')
        0b00011000 ,
        0b00101000 ,
        0b01001000 ,
        0b11111110 ,
        0b00001000 ,
    },

    { //  ASCII 0x35 ('5')
        0b11100100 ,
        0b10100010 ,
        0b10100010 ,
        0b10100010 ,
        0b10011100 ,
    },

    { //  ASCII 0x36 ('6')
        0b00111100 ,
        0b01010010 ,
        0b10010010 ,
        0b10010010 ,
        0b00001100 ,
    },

    { //  ASCII 0x37 ('7')
        0b10000000 ,
        0b10001110 ,
        0b10010000 ,
        0b10100000 ,
        0b11000000 ,
    },

    { //  ASCII 0x38 ('8')
        0b01101100 ,
        0b10010010 ,
        0b10010010 ,
        0b10010010 ,
        0b01101100 ,
    },

    { //  ASCII 0x39 ('9')
        0b01100000 ,
        0b10010010 ,
        0b10010010 ,
        0b10010100 ,
        0b01111000 ,
    },

    { //  ASCII 0x3a (':')
        0b00000000 ,
        0b01101100 ,
        0b01101100 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x3b (';')
        0b00000000 ,
        0b01101010 ,
        0b01101100 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x3c ('<')
        0b00000000 ,
        0b00010000 ,
        0b00101000 ,
        0b01000100 ,
        0b10000010 ,
    },

    { //  ASCII 0x3d ('=')
        0b00101000 ,
        0b00101000 ,
        0b00101000 ,
        0b00101000 ,
        0b00101000 ,
    },

    { //  ASCII 0x3e ('>')
        0b10000010 ,
        0b01000100 ,
        0b00101000 ,
        0b00010000 ,
        0b00000000 ,
    },

    { //  ASCII 0x3f ('?')
        0b01000000 ,
        0b10000000 ,
        0b10001010 ,
        0b10010000 ,
        0b01100000 ,
    },

    { //  ASCII 0x40 ('@')
        0b01001100 ,
        0b10010010 ,
        0b10011110 ,
        0b10000010 ,
        0b01111100 ,
    },

    { //  ASCII 0x41 ('A')
        0b01111110 ,
        0b10001000 ,
        0b10001000 ,
        0b10001000 ,
        0b01111110 ,
    },

    { //  ASCII 0x42 ('B')
        0b11111110 ,
        0b10010010 ,
        0b10010010 ,
        0b10010010 ,
        0b01101100 ,
    },

    { //  ASCII 0x43 ('C')
        0b01111100 ,
        0b10000010 ,
        0b10000010 ,
        0b10000010 ,
        0b01000100 ,
    },

    { //  ASCII 0x44 ('D')
        0b11111110 ,
        0b10000010 ,
        0b10000010 ,
        0b01000100 ,
        0b00111000 ,
    },

    { //  ASCII 0x45 ('E')
        0b11111110 ,
        0b10010010 ,
        0b10010010 ,
        0b10010010 ,
        0b10000010 ,
    },

    { //  ASCII 0x46 ('F')
        0b11111110 ,
        0b10010000 ,
        0b10010000 ,
        0b10000000 ,
        0b10000000 ,
    },

    { //  ASCII 0x47 ('G')
        0b01111100 ,
        0b10000010 ,
        0b10000010 ,
        0b10001010 ,
        0b01001100 ,
    },

    { //  ASCII 0x48 ('H')
        0b11111110 ,
        0b00010000 ,
        0b00010000 ,
        0b00010000 ,
        0b11111110 ,
    },

    { //  ASCII 0x49 ('I')
        0b00000000 ,
        0b10000010 ,
        0b11111110 ,
        0b10000010 ,
        0b00000000 ,
    },

    { //  ASCII 0x4a ('J')
        0b00000100 ,
        0b00000010 ,
        0b10000010 ,
        0b11111100 ,
        0b10000000 ,
    },

    { //  ASCII 0x4b ('K')
        0b11111110 ,
        0b00010000 ,
        0b00101000 ,
        0b01000100 ,
        0b10000010 ,
    },

    { //  ASCII 0x4c ('L')
        0b11111110 ,
        0b00000010 ,
        0b00000010 ,
        0b00000010 ,
        0b00000010 ,
    },

    { //  ASCII 0x4d ('M')
        0b11111110 ,
        0b01000000 ,
        0b00100000 ,
        0b01000000 ,
        0b11111110 ,
    },

    { //  ASCII 0x4e ('N')
        0b11111110 ,
        0b00100000 ,
        0b00010000 ,
        0b00001000 ,
        0b11111110 ,
    },

    { //  ASCII 0x4f ('O')
        0b01111100 ,
        0b10000010 ,
        0b10000010 ,
        0b10000010 ,
        0b01111100 ,
    },

    { //  ASCII 0x50 ('P')
        0b11111110 ,
        0b10010000 ,
        0b10010000 ,
        0b10010000 ,
        0b01100000 ,
    },

    { //  ASCII 0x51 ('Q')
        0b01111100 ,
        0b10000010 ,
        0b10001010 ,
        0b10000100 ,
        0b01111010 ,
    },

    { //  ASCII 0x52 ('R')
        0b11111110 ,
        0b10010000 ,
        0b10011000 ,
        0b10010100 ,
        0b01100010 ,
    },

    { //  ASCII 0x53 ('S')
        0b01100010 ,
        0b10010010 ,
        0b10010010 ,
        0b10010010 ,
        0b10001100 ,
    },

    { //  ASCII 0x54 ('T')
        0b10000000 ,
        0b10000000 ,
        0b11111110 ,
        0b10000000 ,
        0b10000000 ,
    },

    { //  ASCII 0x55 ('U')
        0b11111100 ,
        0b00000010 ,
        0b00000010 ,
        0b00000010 ,
        0b11111100 ,
    },

    { //  ASCII 0x56 ('V')
        0b11111000 ,
        0b00000100 ,
        0b00000010 ,
        0b00000100 ,
        0b11111000 ,
    },

    { //  ASCII 0x57 ('W')
        0b11111110 ,
        0b00000100 ,
        0b00011000 ,
        0b00000100 ,
        0b11111110 ,
    },

    { //  ASCII 0x58 ('X')
        0b11000110 ,
        0b00101000 ,
        0b00010000 ,
        0b00101000 ,
        0b11000110 ,
    },

    { //  ASCII 0x59 ('Y')
        0b11000000 ,
        0b00100000 ,
        0b00011110 ,
        0b00100000 ,
        0b11000000 ,
    },

    { //  ASCII 0x5a ('Z')
        0b10000110 ,
        0b10001010 ,
        0b10010010 ,
        0b10100010 ,
        0b11000010 ,
    },

    { //  ASCII 0x5b ('[')
        0b00000000 ,
        0b00000000 ,
        0b11111110 ,
        0b10000010 ,
        0b10000010 ,
    },

    { //  ASCII 0x5c ('\')
        0b01000000 ,
        0b00100000 ,
        0b00010000 ,
        0b00001000 ,
        0b00000100 ,
    },

    { //  ASCII 0x5d (']')
        0b10000010 ,
        0b10000010 ,
        0b11111110 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x5e ('^')
        0b00100000 ,
        0b01000000 ,
        0b10000000 ,
        0b01000000 ,
        0b00100000 ,
    },

    { //  ASCII 0x5f ('_')
        0b00000010 ,
        0b00000010 ,
        0b00000010 ,
        0b00000010 ,
        0b00000010 ,
    },

    { //  ASCII 0x60 ('`')
        0b00000000 ,
        0b10000000 ,
        0b01000000 ,
        0b00100000 ,
        0b00000000 ,
    },

    { //  ASCII 0x61 ('a')
        0b00000100 ,
        0b00101010 ,
        0b00101010 ,
        0b00101010 ,
        0b00011110 ,
    },

    { //  ASCII 0x62 ('b')
        0b11111110 ,
        0b00010010 ,
        0b00100010 ,
        0b00100010 ,
        0b00011100 ,
    },

    { //  ASCII 0x63 ('c')
        0b00011100 ,
        0b00100010 ,
        0b00100010 ,
        0b00100010 ,
        0b00000100 ,
    },

    { //  ASCII 0x64 ('d')
        0b00011100 ,
        0b00100010 ,
        0b00100010 ,
        0b00010010 ,
        0b11111110 ,
    },

    { //  ASCII 0x65 ('e')
        0b00011100 ,
        0b00101010 ,
        0b00101010 ,
        0b00101010 ,
        0b00011000 ,
    },

    { //  ASCII 0x66 ('f')
        0b00010000 ,
        0b01111110 ,
        0b10010000 ,
        0b10000000 ,
        0b01000000 ,
    },

    { //  ASCII 0x67 ('g')
        0b00010000 ,
        0b00101000 ,
        0b00101010 ,
        0b00101010 ,
        0b00111100 ,
    },

    { //  ASCII 0x68 ('h')
        0b11111110 ,
        0b00010000 ,
        0b00100000 ,
        0b00100000 ,
        0b00011110 ,
    },

    { //  ASCII 0x69 ('i')
        0b00000000 ,
        0b00100010 ,
        0b10111110 ,
        0b00000010 ,
        0b00000000 ,
    },

    { //  ASCII 0x6a ('j')
        0b00000100 ,
        0b00000010 ,
        0b00100010 ,
        0b10111100 ,
        0b00000000 ,
    },

    { //  ASCII 0x6b ('k')
        0b00000000 ,
        0b11111110 ,
        0b00001000 ,
        0b00010100 ,
        0b00100010 ,
    },

    { //  ASCII 0x6c ('l')
        0b00000000 ,
        0b10000010 ,
        0b11111110 ,
        0b00000010 ,
        0b00000000 ,
    },

    { //  ASCII 0x6d ('m')
        0b00111110 ,
        0b00100000 ,
        0b00011000 ,
        0b00100000 ,
        0b00011110 ,
    },

    { //  ASCII 0x6e ('n')
        0b00111110 ,
        0b00010000 ,
        0b00100000 ,
        0b00100000 ,
        0b00011110 ,
    },

    { //  ASCII 0x6f ('o')
        0b00011100 ,
        0b00100010 ,
        0b00100010 ,
        0b00100010 ,
        0b00011100 ,
    },

    { //  ASCII 0x70 ('p')
        0b00111110 ,
        0b00101000 ,
        0b00101000 ,
        0b00101000 ,
        0b00010000 ,
    },

    { //  ASCII 0x71 ('q')
        0b00010000 ,
        0b00101000 ,
        0b00101000 ,
        0b00011000 ,
        0b00111110 ,
    },

    { //  ASCII 0x72 ('r')
        0b00111110 ,
        0b00010000 ,
        0b00100000 ,
        0b00100000 ,
        0b00010000 ,
    },

    { //  ASCII 0x73 ('s')
        0b00010010 ,
        0b00101010 ,
        0b00101010 ,
        0b00101010 ,
        0b00000100 ,
    },

    { //  ASCII 0x74 ('t')
        0b00100000 ,
        0b11111100 ,
        0b00100010 ,
        0b00000010 ,
        0b00000100 ,
    },

    { //  ASCII 0x75 ('u')
        0b00111100 ,
        0b00000010 ,
        0b00000010 ,
        0b00000100 ,
        0b00111110 ,
    },

    { //  ASCII 0x76 ('v')
        0b00111000 ,
        0b00000100 ,
        0b00000010 ,
        0b00000100 ,
        0b00111000 ,
    },

    { //  ASCII 0x77 ('w')
        0b00111100 ,
        0b00000010 ,
        0b00001100 ,
        0b00000010 ,
        0b00111100 ,
    },

    { //  ASCII 0x78 ('x')
        0b00100010 ,
        0b00010100 ,
        0b00001000 ,
        0b00010100 ,
        0b00100010 ,
    },

    { //  ASCII 0x79 ('y')
        0b00110000 ,
        0b00001010 ,
        0b00001010 ,
        0b00001010 ,
        0b00111100 ,
    },

    { //  ASCII 0x7a ('z')
        0b00100010 ,
        0b00100110 ,
        0b00101010 ,
        0b00110010 ,
        0b00100010 ,
    },

    { //  ASCII 0x7b ('{')
        0b00000000 ,
        0b00010000 ,
        0b01101100 ,
        0b10000010 ,
        0b00000000 ,
    },

    { //  ASCII 0x7c ('|')
        0b00000000 ,
        0b00000000 ,
        0b11111110 ,
        0b00000000 ,
        0b00000000 ,
    },

    { //  ASCII 0x7d ('}')
        0b00000000 ,
        0b10000010 ,
        0b01101100 ,
        0b00010000 ,
        0b00000000 ,
    },

    { //  ASCII 0x7e ('~')
        0b00010000 ,
        0b00010000 ,
        0b01010100 ,
        0b00111000 ,
        0b00010000 ,
    },

    { //  ASCII 0x7f
        0b00010000 ,
        0b00111000 ,
        0b01010100 ,
        0b00010000 ,
        0b00010000 ,
    },
};


// Size of array from https://stackoverflow.com/a/18078435/3152071
template<class T, size_t N>
constexpr size_t size(T (&)[N]) { return N; }

// Send the pixels to form the specified char, not including interchar space
// skip is the number of pixels to skip at the begining to enable sub-char smooth scrolling

// TODO: Subtract the offset from the char before starting the send sequence to save time if nessisary
// TODO: Also could pad the begining of the font table to aovid the offset subtraction at the cost of 20*8 bytes of progmem
// TODO: Could pad all chars out to 8 bytes wide to turn the the multiply by FONT_WIDTH into a shift 

static inline void sendChar( uint8_t c ,  uint8_t skip , uint8_t r,  uint8_t g,  uint8_t b ) {

  // Check if we have a font entry for this character and return if not

  if ( c  < ASCII_OFFSET || c > ASCII_OFFSET + size( fontdata ) ) {
    return;
  }

  const uint8_t *charbase = fontdata[ c - ASCII_OFFSET ]  ; 

  uint8_t col= FONT_WIDTH; 

  while (skip && col) {    
      charbase++;
      skip--;
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
  
         sendString( m , step , 0x00, 0x00 , 0x42 );    // Nice and not-too-bright blue hue
        
         sei();
         
  
      }

    m++;

  }


}
