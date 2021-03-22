// Creates a scrolling ticker tape using WS2812B Neopixel strips. 

// COLOR:
// Allows you to change the color by embededing the command "#rgb" into the string, where r,g, and b are letters A-F indicaing color intensity, so
// the string "#555This is #FAARed#555 and this is #AAFBlue#555." would prinnt the words "Red" and "Blue" in thier respecive colors.
// The default color is #CAA, which is a dim red. 
// Note that the color values must be capital and between A and F. Other values have unpredicable results. 

// BLINKING:
// Allows you to make text blink by embdeding the command "!p" where p is a letter A-N and indictaes the blink pattern according to this table (1=on, 0=off)...
// A=0000, B=0001, C=0010, D=0011, E=0100, F=0101, G=0111, H=1000, I=1010, J=1011, K=1100, L=1101, M=1101, and N=1111, so
// the command !D will blink every other step, while !I will blink on and off every step.
// The default blink rate is !N which is always no (no blink).
// Note that the rate values must be capital and between A and N. Other values have unpredicable results. 

// You should have 7 LED strips with the "Data In" pin for each connected to the D1-D7 pins on an Arduino Uno. (They also need power and ground connections)
// The Arduino listens for text on its built in serial port at 960bd,n,8,1 and then scrolls it out on the LEDs. It uses a buffer to the scrolling speed can be smoothed out.
// To send the serial data, you can connect the Arduino to a computer via USB and then use the Arduino Serial Monitor. Set the monitor to 9600 and "No Line Ending".
// Type stuff in the top bar and press enter and watch it scroll out on the LEDs. 
// You can also connect other serial devices like a bluetooth HC-05 to the RX pin on the Arduino. 
// For more info see the full article at http://wp.josh.com/Build-a-giant-scrolling-ticker-tape-from-WS2812B-Neopixels-and-an-Arduino-Uno

// Change this to be at least as long as your pixel string (too long will work fine, just be a little slower)

#define PIXEL_COUNT 60      // Length of the strings in pixels. I am using a 1 meter long strings that have 60 LEDs per meter. 


#define MAX_DELAY_MS 50    // Max time in ms for each frame while scrolling. Lower numbers make for faster scrolling (until we run out of speed).
                           // Note that we automatically start speeding up when the buffer starts getting full and then slow down again when it starts getting empty. 

static const byte onBits=0b11111110;      // Which digital pins have LED strips attached? 
                                          // If you do not want to use all 8 pins, you can mask off the ones you don't want to use with 0's.
                                          // Here we mask off bit 0 so we can use the serial RX that is also on this pin on an Arduino Uno. 


// Define the color we will send for on pixels. Each value is a byte 0-255. 

#define COLOR_R 0x00                                          
#define COLOR_G 0x00                                          
#define COLOR_B 0x20     
//#define COLOR_W 0x00     // Uncomment this line if you are using RGBW LED strips

/*------------------- FONT CUT TOP HERE -------------------------------*/
#define ASCII_OFFSET (0x20)     // ASCII code of 1st char in font array
#define ASCII_NONPRINT (0x80)   // ASCII code of char to show for chars not included in the font (could also be a space)
#define FONT_WIDTH 6
const byte fontdata[][FONT_WIDTH] PROGMEM = {
    { // ASCII 0x20 (' ')
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x21 ('!')
       0b01110100,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x22 ('"')
       0b11000000,
       0b00000000,
       0b11000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x23 ('#')
       0b00101000,
       0b01111100,
       0b00101000,
       0b01111100,
       0b00101000,
       0b00000000,
    },
    { // ASCII 0x24 ('$')
       0b01110100,
       0b01010100,
       0b11111110,
       0b01010100,
       0b01011100,
       0b00000000,
    },
    { // ASCII 0x25 ('%')
       0b01000100,
       0b00001000,
       0b00010000,
       0b00100000,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x26 ('&')
       0b00101000,
       0b01010100,
       0b01010100,
       0b00001000,
       0b00010100,
       0b00000000,
    },
    { // ASCII 0x27 (''')
       0b11000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x28 ('(')
       0b00111000,
       0b01000100,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x29 (')')
       0b01000100,
       0b00111000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x2A ('*')
       0b10000000,
       0b11000000,
       0b10000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x2B ('+')
       0b00010000,
       0b00010000,
       0b01111100,
       0b00010000,
       0b00010000,
       0b00000000,
    },
    { // ASCII 0x2C (',')
       0b00000110,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x2D ('-')
       0b00010000,
       0b00010000,
       0b00010000,
       0b00010000,
       0b00010000,
       0b00000000,
    },
    { // ASCII 0x2E ('.')
       0b00000100,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x2F ('/')
       0b00001100,
       0b00010000,
       0b01100000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x30 ('0')
       0b01111100,
       0b01001100,
       0b01010100,
       0b01100100,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x31 ('1')
       0b00100100,
       0b01111100,
       0b00000100,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x32 ('2')
       0b01001100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b00100100,
       0b00000000,
    },
    { // ASCII 0x33 ('3')
       0b01000100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01101100,
       0b00000000,
    },
    { // ASCII 0x34 ('4')
       0b01111000,
       0b00001000,
       0b00011100,
       0b00001000,
       0b00001000,
       0b00000000,
    },
    { // ASCII 0x35 ('5')
       0b01110100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01001000,
       0b00000000,
    },
    { // ASCII 0x36 ('6')
       0b01111100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01011100,
       0b00000000,
    },
    { // ASCII 0x37 ('7')
       0b01000000,
       0b01000000,
       0b01001100,
       0b01010000,
       0b01100000,
       0b00000000,
    },
    { // ASCII 0x38 ('8')
       0b01111100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x39 ('9')
       0b01110100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x3A (':')
       0b01000100,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x3B (';')
       0b01000110,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x3C ('<')
       0b00010000,
       0b00101000,
       0b01000100,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x3D ('=')
       0b00101000,
       0b00101000,
       0b00101000,
       0b00101000,
       0b00101000,
       0b00000000,
    },
    { // ASCII 0x3E ('>')
       0b01000100,
       0b00101000,
       0b00010000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x3F ('?')
       0b00100000,
       0b01000000,
       0b01010100,
       0b00100000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x40 ('@')
       0b01111100,
       0b01000100,
       0b01010100,
       0b01010100,
       0b01110100,
       0b00000000,
    },
    { // ASCII 0x41 ('A')
       0b01111100,
       0b01001000,
       0b01001000,
       0b01001000,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x42 ('B')
       0b01111100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01101100,
       0b00000000,
    },
    { // ASCII 0x43 ('C')
       0b01111100,
       0b01000100,
       0b01000100,
       0b01000100,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x44 ('D')
       0b01111100,
       0b01000100,
       0b01000100,
       0b01000100,
       0b00111000,
       0b00000000,
    },
    { // ASCII 0x45 ('E')
       0b01111100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x46 ('F')
       0b01111100,
       0b01010000,
       0b01010000,
       0b01010000,
       0b01000000,
       0b00000000,
    },
    { // ASCII 0x47 ('G')
       0b01111100,
       0b01000100,
       0b01000100,
       0b01010100,
       0b01011100,
       0b00000000,
    },
    { // ASCII 0x48 ('H')
       0b01111100,
       0b00010000,
       0b00010000,
       0b00010000,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x49 ('I')
       0b01000100,
       0b01000100,
       0b01111100,
       0b01000100,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x4A ('J')
       0b00001100,
       0b00000100,
       0b00000100,
       0b01000100,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x4B ('K')
       0b01111100,
       0b00010000,
       0b00010000,
       0b00101000,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x4C ('L')
       0b01111100,
       0b00000100,
       0b00000100,
       0b00000100,
       0b00000100,
       0b00000000,
    },
    { // ASCII 0x4D ('M')
       0b01111100,
       0b00100000,
       0b00010000,
       0b00100000,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x4E ('N')
       0b01111100,
       0b00100000,
       0b00010000,
       0b00001000,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x4F ('O')
       0b00111000,
       0b01000100,
       0b01000100,
       0b01000100,
       0b00111000,
       0b00000000,
    },
    { // ASCII 0x50 ('P')
       0b01111100,
       0b01010000,
       0b01010000,
       0b01010000,
       0b00100000,
       0b00000000,
    },
    { // ASCII 0x51 ('Q')
       0b01111000,
       0b01001000,
       0b01001100,
       0b01001000,
       0b01111000,
       0b00000000,
    },
    { // ASCII 0x52 ('R')
       0b01111100,
       0b01010000,
       0b01010000,
       0b01010000,
       0b00101100,
       0b00000000,
    },
    { // ASCII 0x53 ('S')
       0b01110100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01011100,
       0b00000000,
    },
    { // ASCII 0x54 ('T')
       0b01000000,
       0b01000000,
       0b01111100,
       0b01000000,
       0b01000000,
       0b00000000,
    },
    { // ASCII 0x55 ('U')
       0b01111100,
       0b00000100,
       0b00000100,
       0b00000100,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x56 ('V')
       0b01100000,
       0b00011000,
       0b00000100,
       0b00011000,
       0b01100000,
       0b00000000,
    },
    { // ASCII 0x57 ('W')
       0b01111000,
       0b00000100,
       0b00011000,
       0b00000100,
       0b01111000,
       0b00000000,
    },
    { // ASCII 0x58 ('X')
       0b01000100,
       0b00101000,
       0b00010000,
       0b00101000,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x59 ('Y')
       0b01100000,
       0b00010000,
       0b00001100,
       0b00010000,
       0b01100000,
       0b00000000,
    },
    { // ASCII 0x5A ('Z')
       0b01000100,
       0b01001100,
       0b01010100,
       0b01100100,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x5B ('[')
       0b01111100,
       0b01000100,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x5C ('\')
       0b01100000,
       0b00010000,
       0b00001100,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x5D (']')
       0b01000100,
       0b01111100,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x5E ('^')
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x5F ('_')
       0b00000100,
       0b00000100,
       0b00000100,
       0b00000100,
       0b00000100,
       0b00000100,
    },
    { // ASCII 0x60 ('`')
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x61 ('a')
       0b01111100,
       0b01001000,
       0b01001000,
       0b01001000,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x62 ('b')
       0b01111100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01101100,
       0b00000000,
    },
    { // ASCII 0x63 ('c')
       0b01111100,
       0b01000100,
       0b01000100,
       0b01000100,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x64 ('d')
       0b01111100,
       0b01000100,
       0b01000100,
       0b01000100,
       0b00111000,
       0b00000000,
    },
    { // ASCII 0x65 ('e')
       0b01111100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x66 ('f')
       0b01111100,
       0b01010000,
       0b01010000,
       0b01010000,
       0b01000000,
       0b00000000,
    },
    { // ASCII 0x67 ('g')
       0b01111100,
       0b01000100,
       0b01000100,
       0b01010100,
       0b01011100,
       0b00000000,
    },
    { // ASCII 0x68 ('h')
       0b01111100,
       0b00010000,
       0b00010000,
       0b00010000,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x69 ('i')
       0b01000100,
       0b01000100,
       0b01111100,
       0b01000100,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x6A ('j')
       0b00001100,
       0b00000100,
       0b00000100,
       0b01000100,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x6B ('k')
       0b01111100,
       0b00010000,
       0b00010000,
       0b00101000,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x6C ('l')
       0b01111100,
       0b00000100,
       0b00000100,
       0b00000100,
       0b00000100,
       0b00000000,
    },
    { // ASCII 0x6D ('m')
       0b01111100,
       0b00100000,
       0b00010000,
       0b00100000,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x6E ('n')
       0b01111100,
       0b00100000,
       0b00010000,
       0b00001000,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x6F ('o')
       0b00111000,
       0b01000100,
       0b01000100,
       0b01000100,
       0b00111000,
       0b00000000,
    },
    { // ASCII 0x70 ('p')
       0b01111100,
       0b01010000,
       0b01010000,
       0b01010000,
       0b00100000,
       0b00000000,
    },
    { // ASCII 0x71 ('q')
       0b01111000,
       0b01001000,
       0b01001100,
       0b01001000,
       0b01111000,
       0b00000000,
    },
    { // ASCII 0x72 ('r')
       0b01111100,
       0b01010000,
       0b01010000,
       0b01010000,
       0b00101100,
       0b00000000,
    },
    { // ASCII 0x73 ('s')
       0b01110100,
       0b01010100,
       0b01010100,
       0b01010100,
       0b01011100,
       0b00000000,
    },
    { // ASCII 0x74 ('t')
       0b01000000,
       0b01000000,
       0b01111100,
       0b01000000,
       0b01000000,
       0b00000000,
    },
    { // ASCII 0x75 ('u')
       0b01111100,
       0b00000100,
       0b00000100,
       0b00000100,
       0b01111100,
       0b00000000,
    },
    { // ASCII 0x76 ('v')
       0b01100000,
       0b00011000,
       0b00000100,
       0b00011000,
       0b01100000,
       0b00000000,
    },
    { // ASCII 0x77 ('w')
       0b01111000,
       0b00000100,
       0b00011000,
       0b00000100,
       0b01111000,
       0b00000000,
    },
    { // ASCII 0x78 ('x')
       0b01000100,
       0b00101000,
       0b00010000,
       0b00101000,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x79 ('y')
       0b01100000,
       0b00010000,
       0b00001100,
       0b00010000,
       0b01100000,
       0b00000000,
    },
    { // ASCII 0x7A ('z')
       0b01000100,
       0b01001100,
       0b01010100,
       0b01100100,
       0b01000100,
       0b00000000,
    },
    { // ASCII 0x7B ('{')
       0b00010000,
       0b01111100,
       0b01000100,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x7C ('|')
       0b01101100,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x7D ('}')
       0b01000100,
       0b01111100,
       0b00010000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
    { // ASCII 0x7E ('~')
       0b10000000,
       0b00000000,
       0b10000000,
       0b00000000,
       0b00000000,
       0b00000000,
    },
};
/*------------------- FONT CUT BOTTOM HERE -------------------------------*/


// The hard part of actually sending data out the strings is below. 

#define RES_NS 500000   // Width of the low gap between bits to cause a frame to latch, from the WS2812B datasheets (recently increased to 50us for newer chips)

// We access the digital pins directly for speed. See https://www.arduino.cc/en/Reference/PortManipulation

#define PIXEL_PORT  PORTD  // Data register of the pins the pixels are connected to
#define PIXEL_DDR   DDRD   // Direction register of the pins the pixels are connected to

// Sends a full 8 bits down all the pins, represening a single color of 1 pixel
// We walk though the 8 bits in colorbyte one at a time. If the bit is 1 then we send the 8 bits of row out. Otherwise we send 0. 
// We send onBits at the first phase of the signal generation. We could just send 0xff, but that mught enable pull-ups on pins that we are not using. 

/// Unforntunately we have to drop to ASM for this so we can interleave the computaions durring the delays, otherwise things get too slow.

// OnBits is the mask of which bits are connected to strips. We pass it on so that we
// do not turn on unused pins becuase this would enable the pullup. Also, hopefully passing this
// will cause the compiler to allocate a Register for it and avoid a reload every pass.

// Assumes interrupts are *off* on entry and returns with them off, but it does turn them on between bits and can
// tolerate being interrupted for up to 5us. 

// Note that I would like to make this function ` __attribute__((always_inline)) `, but doing so cuases an endless loop someplace.
// If you can figure out why, LMK and I'll send you a tee shirt.

// At 16Mhz, each cycle is 62.5ns. We will aim for...
// T0H 375ns =  6 cycles
// T1H 750ns = 12 cycles
// T1L 375ns =  6 cycles

// Note that forcing this function inline actually slows things down between sends!

static void inline  sendBitx8(  const byte row , const byte colorbyte , const byte onBits ) {  
              
    __asm__ __volatile__ (
      
      // Ok, we will use __tmp_reg as both our bitwalker and our loop counter. We start at 0b10000000 and shift the 1 bit down until it it gone.
      // Unfortunately there is no way to load an immedeate value into __tmp_reg__ so this messy sec/ror is the best I could come up with. Is there a better way?
            
      "mov __tmp_reg__,__zero_reg__ \n\t"           // We will walk this bit down 8 times to test bits in colorbyte and also as our loop counter
      "sec \n\t"
      "ror __tmp_reg__ \n\t"           // We will walk this bit down 8 times to test bits in colorbyte and also as our loop counter

      "L_%=: \n\t"  

            
      "out %[port], %[onBits] \n\t"                 // Send either T0H or the first part of T1H. Onbits is a mask of which bits have strings attached.

      // Next determine if we are going to be sending 1s or 0s based on the current bit in the color....
       
      "push __tmp_reg__ \n\t"                       // (2 cycles)  - I know it seems silly to push this, but it saves a register and we have time to waste here anyway.
      "and __tmp_reg__, %[colorbyte] \n\t"          // (1 cycles)  - is the current bit in the color byte set?
      
      "brne ON_%= \n\t"                             // (1 cycles) - bit in color is 0, then send full zero row (takes 2 cycles if branch taken, count the extra 1 on the target line)

      "nop \n\t "                                   // (1 cycles) - Balances out the extra cycle on the other branch path

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (6 cycles) - T0H = 375ns

      "rjmp NEXT_%= \n\t"                           // (2 cycles) 


      "ON_%=: \n\r"                                 // (1 cycles) - Note that we land here becuase of breq, which takes takes 2 cycles 

      
      // If we get here, then we want to send a 1 for every row that has an ON dot...
      // So if there is a 1 in [row] then the output will still high, otherwise it will go low
      // making a short zero bit signal. 
      "out %[port], %[row]   \n\t"                  // (1 cycles) - set the output bits to [row] This is phase for T0H-T1H.
                                                    // ==========
                                                    // (6 cycles) - T0H (Phase #1) 4 cycles / 16Mhz = 375 nanoseconds. We should be able to get by with 200ns, but I found a WS2813 that says 300ns. 

                                                    // Right here the 1 bits are still high.

      "nop \n\t nop \n\t "                          // (2 cycles) 

                  
      "NEXT_%=: \n\t"                               // Either way we got here, we are at 8 cycles. We need to get to 12. 


      "pop __tmp_reg__  \n\t"                       // (2 cycles)  -  Pop our bitwalker that we pushed above

      "nop \n\t "                                   // (1 cycles) 

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (12 cycles)- T1H (Phase #2 ) 12 cycles / 16Mhz = 750ns      

      // OK we are done sending this set of bits. Now we need a bit of space for time between bits (T1L 375ns, 6 cycles) 


      "nop \n\t nop \n\t "                          // (2 cycles) 

      "nop \n\t "                                   // (1 cycles)      

      "lsr __tmp_reg__ \n\t "                       // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag
                  
      "brcc L_%= \n\t"                              // (2 cycles if loop followed) Exit if carry bit is set as a result of us walking all 8 bits. 
                                                    // If above loop is taken, then full 6 cycles for T1L

                                                    // Above is 6 cycles including either the return + next call , or the brcc branch + cli at top

      //"break \n\r"

          
      ::                                          // No outputs
      [port]    "I" (_SFR_IO_ADDR(PIXEL_PORT)),
      [row]   "d" (row),
      [onBits]   "d" (onBits),
      [colorbyte]   "d" (colorbyte )             // Phase 2 of the signal where the actual data bits show up.                
      
    );
                                  
    // Note that the inter-bit gap can be as long as you want as long as it doesn't exceed the reset timeout (which is a long time)
    
} 


// Just wait long enough without sending any bits to cause the pixels to latch and display the last sent frame
// This is the "RESET" period in the WS2812B datasheets

void show() {
  delayMicroseconds( (RES_NS / 1000UL) + 1);       // Round up since the delay must be _at_least_ this long (too short might not work, too long not a problem)
}


// Size of array from https://stackoverflow.com/a/18078435/3152071
template<class T, size_t N>
constexpr size_t size(T (&)[N]) { return N; }


// Send 3 bytes of color data (R,G,B) for a signle pixel down all the connected strings at the same time
// A 1 bit in "row" means send the color, a 0 bit means send black. 
// Assumes interrupts are OFF, but turns them on briefly between bytes since this is when we have the most time to
// allow some latency - about 5us for older WS2812Bs, much longer for newer ones. Our serial ISR takes much less than 5us,
// so should be no problem. 

static __attribute__((always_inline)) inline void sendCol( byte colBits  ) {

  sendBitx8( colBits , COLOR_G , onBits);    // WS2812 takes colors in GRB order
  sei();
  cli();
  sendBitx8( colBits , COLOR_R , onBits);    // WS2812 takes colors in GRB order
  sei();
  cli();
  sendBitx8( colBits , COLOR_B , onBits);    // WS2812 takes colors in GRB order  
  #ifdef COLOR_W 
    sendBitx8( colBits , COLOR_W , onBits);    // White for RGBW strips. Uncomment line above to use these strips. 
  #endif
  
}

// Here we define stuff to recieve the serial data. I guess we could have used the Arduino Serial class
// here, but that code is more general, whereas we know exactly what we need to do so can get right to the point.
// Also we do not want to turn on the TX pin since we might be using that for LEDs.

// Must be big enough to hold serial data coming in while we are showing a message on the LEDs

#define BUFFER_LEN 1000

byte buffer[ BUFFER_LEN ];

volatile byte * volatile buffer_head = buffer;      // Newly recieved bytes go here
volatile byte *buffer_tail = buffer;                // Points to the first currently displayed (leftmost) char

const byte *buffer_last = buffer +BUFFER_LEN -1;    // Remember the end of the buffer to we can test for it when incrementing to detect wrap at end

// Increment the specified pointer with wrap if we get to the end
// Two versions to match volatile and non-volatile. I could not figure out how to do this with a template and one function, can you?

 byte *increment_buffer_ptr( byte *ptr) {
  if (ptr!=buffer_last) {
    return ptr+1;
  } else {
    return buffer;
  }
}

 volatile byte *increment_buffer_ptr( volatile byte * volatile ptr) {
  if (ptr!=buffer_last) {
    return ptr+1;
  } else {
    return buffer;
  }
}

// Return number of bytes currently in buffer

unsigned buffer_count() {

  if (buffer_head >= buffer_tail) {
    return buffer_head-buffer_tail;
  } else {
    // This means that the head has already wrapped around so now tail as to catch up
    // We figure this out by adding the byte from the tail to the end, and then also from the beginging to the head
    return (buffer_last - buffer_tail +1) + (buffer_head -buffer);
  }
  
}


// Programatically add a char to the end of the buffer 
// Inlined this since it is called from the serial ISR so we want to be quick about it. 

void inline stuff_buffer( const byte b ) {
  
  volatile byte * const new_head = increment_buffer_ptr(buffer_head);        // Compute the new head after this char

  if ( new_head != buffer_tail)  {             // Room for another char? If not, it is siently dropped
      *buffer_head = b;
      buffer_head = new_head;
  }   
  
}


// Programatically add some text to the end of the buffer 

void stuff_buffer( const char *m ) {

  while (*m) stuff_buffer(*(m++));
  
}


// Interrupt handler, fired whenever a byte comes in on the RX pin. 
// We grab the byte and stick it in our buffer (if we have room for it)

ISR(USART_RX_vect) {

  byte b = UDR0;      // Get newly recieved byte from the USART

  stuff_buffer(b);    // Shoul be inlined above to avoid extra call/return in the ISR

}

// Enable recieve on RX pin at 9600,n,8,1 

void init_serial() {
  UBRR0 = 103;            // 9600bd at 16Mhz. From datahseet DS40001909A-page 222
  UCSR0B |= 1 << RXEN0;   // Enable reciever on RX pin
  UCSR0B |= 1 << RXCIE0;  // Enable recieve complete interrupt
}

// true if char is in the font

static constexpr byte isValidChar( const byte b ) {
  return (b>=ASCII_OFFSET) && (b<(ASCII_OFFSET+size(fontdata)));
}

// Returns a pointer to the first column of the specified ascii char in the font
// if the char is not in the font, then is returns the first col of ASCII_NONPRINT
// Note this is retruning a pointer inside fontdata which is in PROGMEM 

static inline const byte *getFirstColOfChar( const byte c ) {

  if (isValidChar( c )) {
  
    return fontdata[ c- ASCII_OFFSET ];

  } else {

    return fontdata[ ASCII_NONPRINT- ASCII_OFFSET ];    // We don not have font data for ths ASCCI, so show the specified non-printable symbol instead
    
  }
  
}

// Send a full batch of data out to the LEDs
// Starts at buffer_tail and keeps sending until all the LEDs are updated.

// shift is the col of the current char to start at. By shifting though we can smooth scrool it across the width of the char. 
// when shift=0 that means start at col 0, which means send the full char width of the first char.

// Returns true there is more left in the buffer that did not fit on the display.

byte updateLEDs( byte shift ) {

  // Here is how we do it: 
  // We start at the tail, which is the char at the leftmost position on the display (also closest to the Arduino)
  // We start at the column specified by "shift" and start sending out the columns until we get to the end of this first char
  // and then we advance to the next char and send all of its columns and keep sending until we have filled the entire LED display (PIXEL_WIDTH).
  // As we advance, we check to make sure we do not go past the head (the newest recieved chars). If we get to the head, we send blank
  // columns to pad out the rest of the display. 
  
  unsigned pixel_count = PIXEL_COUNT;  // Fill all the LED pixels. Note it is actually ok to send too many pixels, just would be slower

  volatile byte *buffer_edge = buffer_tail;    // Start walking buffer_edge to the end of the string starting from tail (this is the letter we are currently sending)

  volatile byte *buffer_head_snap = buffer_head;    // take a snapshot of the head becuase it can update in the background when new serial data comes in

  // Sorry now comes several ugly optimizations. The code was much clearer, but it was too slow for the pixels. 
  // Check out the clean version here https://github.com/bigjosh/MacroMarquee/blob/8fc8c4b1d1a43357b970d4f17a197309e4133168/Arduino/Tickertape/Tickertape.ino#L1054
  
  // First we step out the leftmost char, which could be shifted...
  const byte *next_font_col =  getFirstColOfChar( *buffer_edge )+shift;    // Start at the shifted column
  byte font_cols_left = FONT_WIDTH - shift ;

  cli();    // Disable ints while we send data to pixels so we do not get interrupted. 

    
  while (font_cols_left && pixel_count) {
    
    sendCol( pgm_read_byte_near( next_font_col++ ));    // Send next col of bits to LEDs. pgm_read stuff is becuase fontdata is PROGMEM.
    pixel_count--;
    
    font_cols_left--;

    if (!font_cols_left) {

      buffer_edge= increment_buffer_ptr( buffer_edge );     // move on to next char
  
      if (buffer_edge == buffer_head_snap) {
        goto FINISHED_BUFFER;                 // Sorry, but can you think of a better way to do this sequence that is not slower? 
      }
  
      next_font_col = getFirstColOfChar( *buffer_edge );
      font_cols_left = FONT_WIDTH;                    // full char    
            
    }

    
  }
 
  FINISHED_BUFFER:

  // Fill any remaining pixels with blanks. This only happens when we first start up and buffer is empty. 

  while (pixel_count--) {
    sendCol(  0  );    // All pixels in column off               
  }

  FINISHED_PIXELS:
    
  sei();  // All done with time critical stuff. 

  // Latch everything we just sent into the pixels so it is actually displayed
  show();

  return buffer_edge != buffer_head_snap ;
}



void setup() {
  init_serial();
  
  PIXEL_DDR |= onBits;         // Set used pins to output mode

  // This seems to reset some stuck pixels and leaves all outputs cleanly low
  PIXEL_PORT |= onBits;       // Set all outputs to 1
  delay( 100);
  PIXEL_PORT &= ~onBits;       // Set all outputs to 0
  delay( 100);
  PIXEL_PORT |= onBits;       // Set all outputs to 1
  delay( 100);
  PIXEL_PORT &= ~onBits;       // Set all outputs to 0
  delay( 100);

  // Show something on startup so we know it is working
  // (you can delete this branding if you are that kind of person)
  stuff_buffer( "SimpleTickertape from JOSH.COM " );
}


byte shift = 0;

unsigned long last_frame_time_ms = 0;

void loop() {  

  byte moreFlag = updateLEDs(  shift );    // Draw the display, see if there is any data beyond the display currently 


  if ( moreFlag )  {                      // If there is more text in the buffer, we will scroll it out 1 column at a time

    // There is more in the buffer to display

    float buffer_fullness = 1.0 *  buffer_count() / BUFFER_LEN;    // How full is the buffer? 0.0-1.0

    // Here we adjust our speed based on how full the buffer is to help empty it quicker when it starts to get full.
    // We actually use the square of the speed so that things stay slow and readable until the buffer really
    // starts to get full. 
    unsigned delay_ms = 1.0* MAX_DELAY_MS * ( 1.0 - (buffer_fullness * buffer_fullness));
    delay( delay_ms ); 

    shift++;                              // Shift current char forward one column

    if (shift == FONT_WIDTH) {            // If we are done with this char, on to the next one
      buffer_tail = increment_buffer_ptr( buffer_tail );
      shift = 0;      
    }
  }


}
