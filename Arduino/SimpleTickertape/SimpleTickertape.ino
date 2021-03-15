// Creates a simplified scrolling ticker tape using WS2812B Neopixel strips. 
// You should have 7 LED strips with the "Data In" pin for each connected to the D1-D7 pins on an Arduino Uno. (They also need power and ground connections)
// The Arduino listens for text on its built in serial port at 960bd,n,8,1 and then scrolls it out on the LEDs. It uses a buffer to the scrolling speed can be smoothed out.
// To send the serial data, you can connect the Arduino to a computer via USB and then use the Arduino Serial Monitor. Set the monitor to 9600 and "No Line Ending".
// Type stuff in the top bar and press enter and watch it scroll out on the LEDs. 
// You can also connect other serial devices like a bluetooth HC-05 to the RX pin on the Arduino. 
// For more info see the full article at http://wp.josh.com/Build-a-giant-scrolling-ticker-tape-from-WS2812B-Neopixels-and-an-Arduino-Uno

// Change this to be at least as long as your pixel string (too long will work fine, just be a little slower)

#define PIXEL_COUNT 60      // Length of the strings in pixels. I am using a 1 meter long strings that have 60 LEDs per meter. 


#define FRAME_DELAY_MS 50    // Minimum time in ms for each frame while scrolling. Lower numbers make for faster scrolling (until we run out of speed).

static const byte onBits=0b11111110;      // Which digital pins have LED strips attached? 
                                          // If you do not want to use all 8 pins, you can mask off the ones you don't want to use with 0's.
                                          // Here we mask off bit 0 so we can use the serial RX that is also on this pin on an Arduino Uno. 


// Define the color we will send for on pixels. Each value is a byte 0-255. 

#define COLOR_R 0x70                                          
#define COLOR_G 0x70                                          
#define COLOR_B 0x70     
//#define COLOR_W 0x00     // Uncomment this line if you are using RGBW LED strips

/*------------------- FONT CUT TOP HERE -------------------------------*/

// This nice 5x7 font from here...
// http://sunge.awardspace.com/glcd-sd/node4.html

// Converted from font to C code with...
// https://github.com/bigjosh/MacroMarquee/blob/master/fontgen/cpp/fontconvert.cpp
// ...but you can also manually create fonts by just typing 1's and 0's if you are patient and you squint. 

// Font details:
// 1) Each char is fixed 5x7 pixels. 
// 2) Each byte is one column.
// 3) Columns are left to right order, leftmost byte is leftmost column of pixels.
// 4) Each column is 8 bits high.
// 5) Bit #7 is top line of char, Bit #1 is bottom.
// 6) Bit #0 is always 0 so that the RX pin can be used for serial input.
// 7) Include optional space to put between chars (or leave out for chars that should touch each other).

// defines ascii characters 0x20-0x7F (32-127)
// PROGMEM after variable name as per https://www.arduino.cc/en/Reference/PROGMEM

#define ASCII_OFFSET (0x20)     // ASCII code of 1st char in font array

#define FONT_WIDTH 6      

const byte fontdata[][FONT_WIDTH] PROGMEM = {

    { // ASCII 0x20 (' ')
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x21 ('!')
       0b00000000,
       0b00000000,
       0b11111010,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x22 ('"')
       0b00000000,
       0b11100000,
       0b00000000,
       0b11100000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x23 ('#')
       0b00101000,
       0b11111110,
       0b00101000,
       0b11111110,
       0b00101000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x24 ('$')
       0b00100100,
       0b01010100,
       0b11111110,
       0b01010100,
       0b01001000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x25 ('%')
       0b11000100,
       0b11001000,
       0b00010000,
       0b00100110,
       0b01000110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x26 ('&')
       0b01101100,
       0b10010010,
       0b10101010,
       0b01000100,
       0b00001010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x27 (''')
       0b00000000,
       0b10100000,
       0b11000000,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x28 ('(')
       0b00000000,
       0b00111000,
       0b01000100,
       0b10000010,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x29 (')')
       0b00000000,
       0b10000010,
       0b01000100,
       0b00111000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x2A ('*')
       0b00010000,
       0b01010100,
       0b00111000,
       0b01010100,
       0b00010000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x2B ('+')
       0b00010000,
       0b00010000,
       0b01111100,
       0b00010000,
       0b00010000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x2C (',')
       0b00000000,
       0b00001010,
       0b00001100,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x2D ('-')
       0b00010000,
       0b00010000,
       0b00010000,
       0b00010000,
       0b00010000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x2E ('.')
       0b00000000,
       0b00000110,
       0b00000110,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x2F ('/')
       0b00000100,
       0b00001000,
       0b00010000,
       0b00100000,
       0b01000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x30 ('0')
       0b01111100,
       0b10001010,
       0b10010010,
       0b10100010,
       0b01111100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x31 ('1')
       0b00000000,
       0b01000010,
       0b11111110,
       0b00000010,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x32 ('2')
       0b01000010,
       0b10000110,
       0b10001010,
       0b10010010,
       0b01100010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x33 ('3')
       0b10000100,
       0b10000010,
       0b10100010,
       0b11010010,
       0b10001100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x34 ('4')
       0b00011000,
       0b00101000,
       0b01001000,
       0b11111110,
       0b00001000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x35 ('5')
       0b11100100,
       0b10100010,
       0b10100010,
       0b10100010,
       0b10011100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x36 ('6')
       0b00111100,
       0b01010010,
       0b10010010,
       0b10010010,
       0b00001100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x37 ('7')
       0b10000000,
       0b10001110,
       0b10010000,
       0b10100000,
       0b11000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x38 ('8')
       0b01101100,
       0b10010010,
       0b10010010,
       0b10010010,
       0b01101100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x39 ('9')
       0b01100000,
       0b10010010,
       0b10010010,
       0b10010100,
       0b01111000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x3A (':')
       0b00000000,
       0b01101100,
       0b01101100,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x3B (';')
       0b00000000,
       0b01101010,
       0b01101100,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x3C ('<')
       0b00000000,
       0b00010000,
       0b00101000,
       0b01000100,
       0b10000010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x3D ('=')
       0b00101000,
       0b00101000,
       0b00101000,
       0b00101000,
       0b00101000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x3E ('>')
       0b10000010,
       0b01000100,
       0b00101000,
       0b00010000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x3F ('?')
       0b01000000,
       0b10000000,
       0b10001010,
       0b10010000,
       0b01100000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x40 ('@')
       0b01001100,
       0b10010010,
       0b10011110,
       0b10000010,
       0b01111100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x41 ('A')
       0b01111110,
       0b10001000,
       0b10001000,
       0b10001000,
       0b01111110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x42 ('B')
       0b11111110,
       0b10010010,
       0b10010010,
       0b10010010,
       0b01101100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x43 ('C')
       0b01111100,
       0b10000010,
       0b10000010,
       0b10000010,
       0b01000100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x44 ('D')
       0b11111110,
       0b10000010,
       0b10000010,
       0b01000100,
       0b00111000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x45 ('E')
       0b11111110,
       0b10010010,
       0b10010010,
       0b10010010,
       0b10000010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x46 ('F')
       0b11111110,
       0b10010000,
       0b10010000,
       0b10000000,
       0b10000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x47 ('G')
       0b01111100,
       0b10000010,
       0b10000010,
       0b10001010,
       0b01001100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x48 ('H')
       0b11111110,
       0b00010000,
       0b00010000,
       0b00010000,
       0b11111110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x49 ('I')
       0b00000000,
       0b10000010,
       0b11111110,
       0b10000010,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x4A ('J')
       0b00000100,
       0b00000010,
       0b10000010,
       0b11111100,
       0b10000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x4B ('K')
       0b11111110,
       0b00010000,
       0b00101000,
       0b01000100,
       0b10000010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x4C ('L')
       0b11111110,
       0b00000010,
       0b00000010,
       0b00000010,
       0b00000010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x4D ('M')
       0b11111110,
       0b01000000,
       0b00100000,
       0b01000000,
       0b11111110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x4E ('N')
       0b11111110,
       0b00100000,
       0b00010000,
       0b00001000,
       0b11111110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x4F ('O')
       0b01111100,
       0b10000010,
       0b10000010,
       0b10000010,
       0b01111100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x50 ('P')
       0b11111110,
       0b10010000,
       0b10010000,
       0b10010000,
       0b01100000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x51 ('Q')
       0b01111100,
       0b10000010,
       0b10001010,
       0b10000100,
       0b01111010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x52 ('R')
       0b11111110,
       0b10010000,
       0b10011000,
       0b10010100,
       0b01100010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x53 ('S')
       0b01100010,
       0b10010010,
       0b10010010,
       0b10010010,
       0b10001100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x54 ('T')
       0b10000000,
       0b10000000,
       0b11111110,
       0b10000000,
       0b10000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x55 ('U')
       0b11111100,
       0b00000010,
       0b00000010,
       0b00000010,
       0b11111100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x56 ('V')
       0b11111000,
       0b00000100,
       0b00000010,
       0b00000100,
       0b11111000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x57 ('W')
       0b11111110,
       0b00000100,
       0b00011000,
       0b00000100,
       0b11111110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x58 ('X')
       0b11000110,
       0b00101000,
       0b00010000,
       0b00101000,
       0b11000110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x59 ('Y')
       0b11000000,
       0b00100000,
       0b00011110,
       0b00100000,
       0b11000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x5A ('Z')
       0b10000110,
       0b10001010,
       0b10010010,
       0b10100010,
       0b11000010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x5B ('[')
       0b00000000,
       0b00000000,
       0b11111110,
       0b10000010,
       0b10000010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x5C ('\')
       0b01000000,
       0b00100000,
       0b00010000,
       0b00001000,
       0b00000100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x5D (']')
       0b10000010,
       0b10000010,
       0b11111110,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x5E ('^')
       0b00100000,
       0b01000000,
       0b10000000,
       0b01000000,
       0b00100000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x5F ('_')
       0b00000010,
       0b00000010,
       0b00000010,
       0b00000010,
       0b00000010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x60 ('`')
       0b00000000,
       0b10000000,
       0b01000000,
       0b00100000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x61 ('a')
       0b00000100,
       0b00101010,
       0b00101010,
       0b00101010,
       0b00011110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x62 ('b')
       0b11111110,
       0b00010010,
       0b00100010,
       0b00100010,
       0b00011100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x63 ('c')
       0b00011100,
       0b00100010,
       0b00100010,
       0b00100010,
       0b00000100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x64 ('d')
       0b00011100,
       0b00100010,
       0b00100010,
       0b00010010,
       0b11111110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x65 ('e')
       0b00011100,
       0b00101010,
       0b00101010,
       0b00101010,
       0b00011000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x66 ('f')
       0b00010000,
       0b01111110,
       0b10010000,
       0b10000000,
       0b01000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x67 ('g')
       0b00010000,
       0b00101000,
       0b00101010,
       0b00101010,
       0b00111100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x68 ('h')
       0b11111110,
       0b00010000,
       0b00100000,
       0b00100000,
       0b00011110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x69 ('i')
       0b00000000,
       0b00100010,
       0b10111110,
       0b00000010,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x6A ('j')
       0b00000100,
       0b00000010,
       0b00100010,
       0b10111100,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x6B ('k')
       0b00000000,
       0b11111110,
       0b00001000,
       0b00010100,
       0b00100010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x6C ('l')
       0b00000000,
       0b10000010,
       0b11111110,
       0b00000010,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x6D ('m')
       0b00111110,
       0b00100000,
       0b00011000,
       0b00100000,
       0b00011110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x6E ('n')
       0b00111110,
       0b00010000,
       0b00100000,
       0b00100000,
       0b00011110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x6F ('o')
       0b00011100,
       0b00100010,
       0b00100010,
       0b00100010,
       0b00011100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x70 ('p')
       0b00111110,
       0b00101000,
       0b00101000,
       0b00101000,
       0b00010000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x71 ('q')
       0b00010000,
       0b00101000,
       0b00101000,
       0b00011000,
       0b00111110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x72 ('r')
       0b00111110,
       0b00010000,
       0b00100000,
       0b00100000,
       0b00010000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x73 ('s')
       0b00010010,
       0b00101010,
       0b00101010,
       0b00101010,
       0b00000100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x74 ('t')
       0b00100000,
       0b11111100,
       0b00100010,
       0b00000010,
       0b00000100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x75 ('u')
       0b00111100,
       0b00000010,
       0b00000010,
       0b00000100,
       0b00111110,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x76 ('v')
       0b00111000,
       0b00000100,
       0b00000010,
       0b00000100,
       0b00111000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x77 ('w')
       0b00111100,
       0b00000010,
       0b00001100,
       0b00000010,
       0b00111100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x78 ('x')
       0b00100010,
       0b00010100,
       0b00001000,
       0b00010100,
       0b00100010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x79 ('y')
       0b00110000,
       0b00001010,
       0b00001010,
       0b00001010,
       0b00111100,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x7A ('z')
       0b00100010,
       0b00100110,
       0b00101010,
       0b00110010,
       0b00100010,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x7B ('{')
       0b00000000,
       0b00010000,
       0b01101100,
       0b10000010,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x7C ('|')
       0b00000000,
       0b00000000,
       0b11111110,
       0b00000000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x7D ('}')
       0b00000000,
       0b10000010,
       0b01101100,
       0b00010000,
       0b00000000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x7E ('~')
       0b00010000,
       0b00010000,
       0b01010100,
       0b00111000,
       0b00010000,
       0b00000000,  // Interchar space
    },
    { // ASCII 0x7F       0b00010000,
       0b00111000,
       0b01010100,
       0b00010000,
       0b00010000,
       0b00000000,  // Interchar space
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

static void sendBitx8(  const byte row , const byte colorbyte , const byte onBits ) {  
              
    __asm__ __volatile__ (


      "L_%=: \n\r"  

            
      "out %[port], %[onBits] \n\t"                 // Send either T0H or the first part of T1H. Onbits is a mask of which bits have strings attached.

      // Next determine if we are going to be sending 1s or 0s based on the current bit in the color....
      "mov r0, %[bitwalker] \n\t"                   // (1 cycles)  - Get ready to check if we should send all zeros      
      "and r0, %[colorbyte] \n\t"                   // (1 cycles)  - is the current bit in the color byte set?
      "breq OFF_%= \n\t"                            // (1 cycles) - bit in color is 0, then send full zero row (takes 2 cycles if branch taken, count the extra 1 on the target line)

      "nop \n\t "                                   // (1 cycles) - Balances out the extra cycle on the other path
      
      // If we get here, then we want to send a 1 for every row that has an ON dot...
      // So if there is a 1 in [row] then the output will still high, otherwise it will go low
      // making a short zero bit signal. 
      "out %[port], %[row]   \n\t"                  // (1 cycles) - set the output bits to [row] This is phase for T0H-T1H.
                                                    // ==========
                                                    // (5 cycles) - T0H (Phase #1) 4 cycles / 16Mhz = 310 nanoseconds. We should be able to get by with 200ns, but I found a WS2813 that says 300ns. 

                                                    
              
      "jmp NEXT_%= \n\t"                            // (3 cycles) 
                                                    // (1 cycles) - The OUT on the next pass of the loop
                                                    // ==========
                                                    // (7 cycles) - T1L
                                                   
                                                          
      "OFF_%=: \n\r"                                // (1 cycles)    Note that we land here becuase of breq, which takes takes 2 cycles

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (4 cycles) - T0H
                                                    

      "nop \n\t \n\t "                              // (1 cycles) - Balances out the extra cycle on the other path
                  
      "NEXT_%=: \n\t"

      "nop \n\t nop \n\t "                          // (2 cycles) 

      "out %[port], __zero_reg__ \n\t"              // (1 cycles) - set the output bits to 0x00 based on the bit in colorbyte. This is phase for T0H-T1H
                                                    // ==========
                                                    // (9 cycles) - T1H (Phase #2 ) 9 cycles / 16Mhz = 560ns      

      // OK we are done sending this set of bits. Now we need a bit of space for time between bits (T1L 600ns) 

      // OK we are done sending this set of bits. Now we need a bit of space for time between bits (T1L 600ns) 
      // We give some interrupts a chance to use this time also. It is the safest place since we just sent a bit and have almost nothing to 
      // do until we send the next one. 
      
      "sei \n\t"                                    // (1 cycles)
      "nop \n\t nop \n\t "                          // (2 cycles) 
      "nop \n\t nop \n\t "                          // (2 cycles)      
      "nop \n\t nop \n\t "                          // (2 cycles)      

      "nop \n\t nop \n\t "                          // (2 cycles)      
      "cli \n\t"                                    // (1 cycles) 

      "ror %[bitwalker] \n\t"                       // (1 cycles) - get ready for next pass. On last pass, the bit will end up in C flag
                  
      "brcc L_%= \n\t"                              // (1 cycles) Exit if carry bit is set as a result of us walking all 8 bits. We assume that the process around us will takw long enough to cover the phase 3 delay

                                                    // Above is at least 9 cycles including either the return + next call , or the brcc branch + cli at top
          
      ::
      [port]    "I" (_SFR_IO_ADDR(PIXEL_PORT)),
      [row]   "d" (row),
      [onBits]   "d" (onBits),
      [colorbyte]   "d" (colorbyte ),     // Phase 2 of the signal where the actual data bits show up.                
      [bitwalker] "r" (0x80)                      // Alocate a register to hold a bit that we will walk down though the color byte

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

static __attribute__((always_inline)) void sendCol( byte colBits  ) {

  sendBitx8( colBits , COLOR_G , onBits);    // WS2812 takes colors in GRB order
  sendBitx8( colBits , COLOR_R , onBits);    // WS2812 takes colors in GRB order
  sendBitx8( colBits , COLOR_B , onBits);    // WS2812 takes colors in GRB order  
  #ifdefined( COLOR_W )
    sendBitx8( colBits , COLOR_W , onBits);    // White for RGBW strips. Uncomment line above to use these strips. 
  #endif
  
}

// Here we define stuff to recieve the serial data. I guess we could have used the Arduino Serial class
// here, but that code is more general, whereas we know exactly what we need to do so can get right to the point.
// Also we do not want to turn on the TX pin since we might be using that for LEDs.

// Must be big enough to hold serial data coming in while we are showing a message on the LEDs

#define BUFFER_LEN 1000

byte buffer[ BUFFER_LEN ];

unsigned volatile buffer_head = 0;       // Newly recieved bytes go here
unsigned volatile buffer_tail = 0;       // Points to the first currently displayed char

// Increment the specified pointer with wrap if we get to the end

#define BUFFER_INC_PTR(x) ( (x==(BUFFER_LEN-1) )? 0 : x+1 )

// Programatically add a char to the end of the buffer 
// I inlined this since it is called from the serial ISR so we want to be quick about it. 

void inline stuff_buffer( const byte b ) {
  
  unsigned new_head = BUFFER_INC_PTR(buffer_head);        // Compute the new head after this char

  if (new_head != buffer_tail)  {             // Room for another char? 
      buffer[buffer_head] = b;
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

  unsigned buffer_edge = buffer_tail;    // Start walking buffer_edge to the end of the string starting from tail (this is the letter we are currently sending)

  unsigned buffer_head_snap = buffer_head;    // take a snapshot of the head becuase it can update in the background when new serial data comes in

  // Sorry now comes several ugly optimizations. The code was much clearer, but it was too slow for the pixels. 
  // Check out the clean version here https://github.com/bigjosh/MacroMarquee/blob/8fc8c4b1d1a43357b970d4f17a197309e4133168/Arduino/Tickertape/Tickertape.ino#L1054
  
  byte *next_font_col = &(fontdata[ buffer[buffer_edge]- ASCII_OFFSET ][shift]);
  byte font_cols_left = FONT_WIDTH - shift;

  // OK pay attention here, this part gets wierd. Our font calculatons below take very close to the max of 5us between bits so we can not afford to
  // get interrupted becuase the additionally delay of the interrupt could make the total delay long enough to cause a reset. So we disable inetrrupt
  // while we are inside updateLEDs() but then enable them durring the spaces between bits in sendBitx8(). Since we do not do any calculation there,
  // we can tollerat up to about 5us of interrupts at that moment. 

  cli();    // Disable ints durring our calculations


  // First we step out the leftmost char, which could be shifted...


  if (buffer_edge != buffer_head_snap) {

    while (pixel_count && font_cols_left ) {

      sendCol( pgm_read_byte_near( next_font_col++ ));    // Send next col of bits to LEDs. pgm_read stuff is becuase fontdata is PROGMEM.
      pixel_count--;
      font_cols_left--;
      
    }

    // Next we send the remaining chars until we run out of pixels or run out of buffer

    buffer_edge= BUFFER_INC_PTR( buffer_edge );     // next char

    if ( buffer_edge != buffer_head_snap) {

      next_font_col = &(fontdata[ buffer[buffer_edge]- ASCII_OFFSET ][0]);
      font_cols_left = FONT_WIDTH;                    // full char

      while (pixel_count) {
    
        sendCol( pgm_read_byte_near( next_font_col++ ));    // Send next col of bits to LEDs. pgm_read stuff is becuase fontdata is PROGMEM.
        pixel_count--;
      
        font_cols_left--;
      
        if (font_cols_left==0) {                        // Finished current char

          buffer_edge= BUFFER_INC_PTR( buffer_edge );   // More on to Next char


          if (buffer_edge == buffer_head_snap) {
            break;                                      // break out of this loop that is sending the middle chars
          }
          
          next_font_col =&(fontdata[ buffer[buffer_edge]- ASCII_OFFSET ][0]);
          font_cols_left = FONT_WIDTH;
        
        }

      }
              
    } 

  }
    
  while (pixel_count--) {
      
    // Blank out any remaining pixels at the end of the display
    // This only happens when we first start up and there is not enough message in the buffer to fill the dispay

    sendCol( 0  );    // All pixels in column off
  }


  sei();  // All done with time critical stuff. 

  // Latch everything we just sent into the pixels so it is actually displayed
  show();

  return buffer_edge != buffer_head_snap ;
}



void setup() {
  PIXEL_DDR |= onBits;         // Set used pins to output mode

  // This seems to reset some stuck pixels and leaves all outputs cleanly low
  PIXEL_PORT |= onBits;       // Set all outputs to 1
  delay( 1000);
  PIXEL_PORT &= ~onBits;       // Set all outputs to 0
  delay( 1000);

  init_serial();

  // Show something on startup so we know it is working
  // (you can delete this branding if you are that kind of person)
  stuff_buffer( "SimpleTickertape from JOSH.COM " );

  updateLEDs( 0 );    // Show startup message.   
}


byte shift = 0;

unsigned long last_frame_time_ms = 0;

void loop() {  

  byte moreFlag = updateLEDs( shift );    // Draw the display, see if there is any data beyond the display currently 
  
  _delay_ms( FRAME_DELAY_MS );            // Slow down a bit so people can see it

  if ( moreFlag )  {                      // If there is more text in the buffer, we will scroll it out 1 column at a time

    shift++;                              // Shift current char forward one column

    if (shift == FONT_WIDTH) {            // If we are done with this char, on to the next one
      buffer_tail = BUFFER_INC_PTR( buffer_tail );
      shift = 0;      
    }
  }


}
