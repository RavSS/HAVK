WITH
   HAVK_Kernel;
USE
   HAVK_Kernel;

PACKAGE HAVK_Kernel.Font
IS
   -- Fonts encompass both ASCII and EASCII.
   TYPE font IS ARRAY(num RANGE 0 .. 255, num RANGE 0 .. 7) OF num;

   -- Going to go with a 8x8 size. Most significant bit is on the left.
   -- Because of left MSB, the loop to draw a line must be reversed.
   Framefont_Width  : CONSTANT num := 8;
   Framefont_Height : CONSTANT num := 8;

   -- TODO: Create the lowercase alphabet and symbols.
   -- A generic font I created for now. No fanciness.
   Framefont : CONSTANT font :=
   (
      48 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000111#,   -- XX   XXX
         2#11001011#,   -- XX  X XX
         2#11010011#,   -- XX X  XX
         2#11100011#,   -- XXX   XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      49 =>
      (
         2#00111000#,   --   XXX
         2#01111000#,   --  XXXX
         2#11011000#,   -- XX XX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      50 =>
      (
         2#11111100#,   -- XXXXXX
         2#11111110#,   -- XXXXXXX
         2#00001110#,   --     XXX
         2#00011110#,   --    XXXX
         2#00111100#,   --   XXXX
         2#11110000#,   -- XXXX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      51 =>
      (
         2#11111110#,   -- XXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00000111#,   --      XXX
         2#11111110#,   -- XXXXXXX
         2#11111110#,   -- XXXXXXX
         2#00000111#,   --      XXX
         2#11111111#,   -- XXXXXXXX
         2#11111110#    -- XXXXXXX
      ),
      52 =>
      (
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00000011#,   --       XX
         2#00000011#,   --       XX
         2#00000011#    --       XX
      ),
      53 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11111100#,   -- XXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00000111#,   --      XXX
         2#11111110#,   -- XXXXXXX
         2#11111100#    -- XXXXXX
      ),
      54 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      55 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00000110#,   --      XX
         2#00001100#,   --     XX
         2#00011000#,   --    XX
         2#00110000#,   --   XX
         2#01100000#,   --  XX
         2#11000000#    -- XX
      ),
      56 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      57 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00000111#,   --      XXX
         2#00001110#,   --     XXX
         2#00011100#    --    XXX
      ),
      65 =>
      (
         2#00011000#,   --    XX
         2#00111100#,   --   XXXX
         2#01100110#,   --  XX  XX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11000011#    -- XX    XX
      ),
      66 =>
      (
         2#11111110#,   -- XXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000111#,   -- XX   XXX
         2#11111110#,   -- XXXXXXX
         2#11111110#,   -- XXXXXXX
         2#11000111#,   -- XX   XXX
         2#11111111#,   -- XXXXXXXX
         2#11111110#    -- XXXXXXX
      ),
      67 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      68 =>
      (
         2#11111100#,   -- XXXXXX
         2#11111110#,   -- XXXXXXX
         2#11000111#,   -- XX   XXX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000111#,   -- XX   XXX
         2#11111110#,   -- XXXXXXX
         2#11111100#    -- XXXXXX
      ),
      69 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      70 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11000000#    -- XX
      ),
      71 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11001111#,   -- XX  XXXX
         2#11001111#,   -- XX  XXXX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      72 =>
      (
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#    -- XX    XX
      ),
      73 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      74 =>
      (
         2#00000011#,   --       XX
         2#00000011#,   --       XX
         2#00000011#,   --       XX
         2#00000011#,   --       XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      75 =>
      (
         2#11000011#,   -- XX    XX
         2#11000110#,   -- XX   XX
         2#11001100#,   -- XX  XX
         2#11111000#,   -- XXXXX
         2#11111000#,   -- XXXXX
         2#11001100#,   -- XX  XX
         2#11000110#,   -- XX   XX
         2#11000011#    -- XX    XX
      ),
      76 =>
      (
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      77 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11011011#    -- XX XX XX
      ),
      78 =>
      (
         2#11100011#,   -- XXX   XX
         2#11110011#,   -- XXXX  XX
         2#11011011#,   -- XX XX XX
         2#11001111#,   -- XX  XXXX
         2#11000111#,   -- XX   XXX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#    -- XX    XX
      ),
      79 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      80 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11000000#,   -- XX
         2#11000000#    -- XX
      ),
      81 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11011011#,   -- XX XX XX
         2#11001111#,   -- XX  XXXX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      82 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11001100#,   -- XX  XX
         2#11000110#,   -- XX   XX
         2#11000011#    -- XX    XX
      ),
      83 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#11000000#,   -- XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00000011#,   --       XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      84 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#    --    XX
      ),
      85 =>
      (
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      86 =>
      (
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#01100110#,   --  XX  XX
         2#00111100#,   --   XXXX
         2#00011000#    --    XX
      ),
      87 =>
      (
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11011011#,   -- XX XX XX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      88 =>
      (
         2#11000011#,   -- XX    XX
         2#01100110#,   --  XX  XX
         2#00111100#,   --   XXXX
         2#00011000#,   --    XX
         2#00111100#,   --   XXXX
         2#01100110#,   --  XX  XX
         2#11000011#,   -- XX    XX
         2#11000011#    -- XX    XX
      ),
      89 =>
      (
         2#11000011#,   -- XX    XX
         2#11000011#,   -- XX    XX
         2#01100110#,   --  XX  XX
         2#00111100#,   --   XXXX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#,   --    XX
         2#00011000#    --    XX
      ),
      90 =>
      (
         2#11111111#,   -- XXXXXXXX
         2#11111111#,   -- XXXXXXXX
         2#00000111#,   --      XXX
         2#00001110#,   --     XXX
         2#00111000#,   --   XXX
         2#11100000#,   -- XXX
         2#11111111#,   -- XXXXXXXX
         2#11111111#    -- XXXXXXXX
      ),
      -- Blank both the control characters and the characters I have not yet
      -- designed into my bitmap font.
      OTHERS => (0, 0, 0, 0, 0, 0, 0, 0)
   );

END HAVK_Kernel.Font;
