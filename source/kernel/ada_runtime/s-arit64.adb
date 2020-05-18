------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . A R I T H _ 6 4                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

WITH Interfaces;
USE Interfaces;

WITH Ada.Unchecked_Conversion;

PACKAGE BODY System.Arith_64 IS

   PRAGMA Suppress (Overflow_Check);
   PRAGMA Suppress (Range_Check);

   SUBTYPE Uns64 IS unsigned_64;
   FUNCTION To_Uns IS NEW Ada.Unchecked_Conversion (int64, uns64);
   FUNCTION To_Int IS NEW Ada.Unchecked_Conversion (uns64, int64);

   SUBTYPE Uns32 IS unsigned_32;

   -----------------------
   -- Local Subprograms --
   -----------------------

   FUNCTION "+"
      (A, B : uns32)
      RETURN uns64 IS (uns64 (A) + uns64 (B));
   FUNCTION "+"
      (A : uns64;
       B : uns32)
      RETURN uns64 IS (A + uns64 (B));
      --  Length doubling additions

   FUNCTION "*"
      (A, B : uns32)
      RETURN uns64 IS (uns64 (A) * uns64 (B));
      --  Length doubling multiplication

   FUNCTION "/"
      (A : uns64;
       B : uns32)
      RETURN uns64 IS (A / uns64 (B));
      --  Length doubling division

   FUNCTION "&"
      (Hi, Lo : uns32)
      RETURN uns64 IS (Shift_Left (uns64 (Hi), 32) OR uns64 (Lo));
      --  Concatenate hi, lo values to form 64-bit result

   FUNCTION "abs"
      (X : int64)
      RETURN uns64 IS
      (IF X = int64'First THEN 2**63 ELSE uns64 (int64'(ABS X)));
      --  Convert absolute value of X to unsigned. Note that we can't just use
      --  the expression of the Else, because it overflows for X = Int64'First.

   FUNCTION "rem"
      (A : uns64;
       B : uns32)
      RETURN uns64 IS (A REM uns64 (B));
      --  Length doubling remainder

   FUNCTION Le3
      (X1, X2, X3 : uns32;
       Y1, Y2, Y3 : uns32)
      RETURN boolean;
      --  Determines if 96 bit value X1&X2&X3 <= Y1&Y2&Y3

   FUNCTION Lo
      (A : uns64)
      RETURN uns32 IS (uns32 (A AND 16#FFFF_FFFF#));
      --  Low order half of 64-bit value

   FUNCTION Hi
      (A : uns64)
      RETURN uns32 IS (uns32 (Shift_Right (A, 32)));
      --  High order half of 64-bit value

   PROCEDURE Sub3
      (X1, X2, X3 : IN OUT uns32;
       Y1, Y2, Y3 :        uns32);
      --  Computes X1&X2&X3 := X1&X2&X3 - Y1&Y1&Y3 with mod 2**96 wrap

   FUNCTION To_Neg_Int
      (A : uns64)
      RETURN int64 WITH
      Inline;
   --  Convert to negative integer equivalent. If the input is in the range
   --  0 .. 2 ** 63, then the corresponding negative signed integer (obtained
   --  by negating the given value) is returned, otherwise constraint error
   --  is raised.

   FUNCTION To_Pos_Int
      (A : uns64)
      RETURN int64 WITH
      Inline;
      --  Convert to positive integer equivalent. If the input is in the range
      --  0 .. 2 ** 63-1, then the corresponding non-negative signed integer is
      --  returned, otherwise constraint error is raised.

   PROCEDURE Raise_Error WITH
      Inline;
   PRAGMA No_Return (Raise_Error);
   --  Raise constraint error with appropriate message

   --------------------------
   -- Add_With_Ovflo_Check --
   --------------------------

   FUNCTION Add_With_Ovflo_Check
      (X, Y : int64)
      RETURN int64
   IS
      R : CONSTANT int64 := To_Int (To_Uns (X) + To_Uns (Y));

   BEGIN
      IF X >= 0
      THEN
         IF Y < 0 OR ELSE R >= 0
         THEN
            RETURN R;
         END IF;

      ELSE -- X < 0
         IF Y > 0 OR ELSE R < 0
         THEN
            RETURN R;
         END IF;
      END IF;

      Raise_Error;
   END Add_With_Ovflo_Check;

   -------------------
   -- Double_Divide --
   -------------------

   PROCEDURE Double_Divide
      (X, Y, Z :     int64;
       Q, R    : OUT int64;
       Round   :     boolean)
   IS
      Xu : CONSTANT uns64 := ABS X;
      Yu : CONSTANT uns64 := ABS Y;

      Yhi : CONSTANT uns32 := Hi (Yu);
      Ylo : CONSTANT uns32 := Lo (Yu);

      Zu  : CONSTANT uns64 := ABS Z;
      Zhi : CONSTANT uns32 := Hi (Zu);
      Zlo : CONSTANT uns32 := Lo (Zu);

      T1, T2     : uns64;
      Du, Qu, Ru : uns64;
      Den_Pos    : boolean;

   BEGIN
      IF Yu = 0 OR ELSE Zu = 0
      THEN
         Raise_Error;
      END IF;

      --  Compute Y * Z. Note that if the result overflows 64 bits unsigned,
      --  then the rounded result is clearly zero (since the dividend is at
      --  most 2**63 - 1, the extra bit of precision is nice here).

      IF Yhi /= 0
      THEN
         IF Zhi /= 0
         THEN
            Q := 0;
            R := X;
            RETURN;
         ELSE
            T2 := Yhi * Zlo;
         END IF;

      ELSE
         T2 := (IF Zhi /= 0 THEN Ylo * Zhi ELSE 0);
      END IF;

      T1 := Ylo * Zlo;
      T2 := T2 + Hi (T1);

      IF Hi (T2) /= 0
      THEN
         Q := 0;
         R := X;
         RETURN;
      END IF;

      Du := Lo (T2) & Lo (T1);

      --  Set final signs (RM 4.5.5(27-30))

      Den_Pos := (Y < 0) = (Z < 0);

      --  Check overflow case of largest negative number divided by 1

      IF X = int64'First AND THEN Du = 1 AND THEN NOT Den_Pos
      THEN
         Raise_Error;
      END IF;

      --  Perform the actual division

      Qu := Xu / Du;
      Ru := Xu REM Du;

      --  Deal with rounding case

      IF Round AND THEN Ru > (Du - uns64'(1)) / uns64'(2)
      THEN
         Qu := Qu + uns64'(1);
      END IF;

      --  Case of dividend (X) sign positive

      IF X >= 0
      THEN
         R := To_Int (Ru);
         Q := (IF Den_Pos THEN To_Int (Qu) ELSE -To_Int (Qu));

         --  Case of dividend (X) sign negative

      ELSE
         R := -To_Int (Ru);
         Q := (IF Den_Pos THEN -To_Int (Qu) ELSE To_Int (Qu));
      END IF;
   END Double_Divide;

   ---------
   -- Le3 --
   ---------

   FUNCTION Le3
      (X1, X2, X3 : uns32;
       Y1, Y2, Y3 : uns32)
      RETURN boolean
   IS
   BEGIN
      IF X1 < Y1
      THEN
         RETURN true;
      ELSIF X1 > Y1
      THEN
         RETURN false;
      ELSIF X2 < Y2
      THEN
         RETURN true;
      ELSIF X2 > Y2
      THEN
         RETURN false;
      ELSE
         RETURN X3 <= Y3;
      END IF;
   END Le3;

   -------------------------------
   -- Multiply_With_Ovflo_Check --
   -------------------------------

   FUNCTION Multiply_With_Ovflo_Check
      (X, Y : int64)
      RETURN int64
   IS
      Xu  : CONSTANT uns64 := ABS X;
      Xhi : CONSTANT uns32 := Hi (Xu);
      Xlo : CONSTANT uns32 := Lo (Xu);

      Yu  : CONSTANT uns64 := ABS Y;
      Yhi : CONSTANT uns32 := Hi (Yu);
      Ylo : CONSTANT uns32 := Lo (Yu);

      T1, T2 : uns64;

   BEGIN
      IF Xhi /= 0
      THEN
         IF Yhi /= 0
         THEN
            Raise_Error;
         ELSE
            T2 := Xhi * Ylo;
         END IF;

      ELSIF Yhi /= 0
      THEN
         T2 := Xlo * Yhi;

      ELSE -- Yhi = Xhi = 0
         T2 := 0;
      END IF;

      --  Here we have T2 set to the contribution to the upper half of the
      --  result from the upper halves of the input values.

      T1 := Xlo * Ylo;
      T2 := T2 + Hi (T1);

      IF Hi (T2) /= 0
      THEN
         Raise_Error;
      END IF;

      T2 := Lo (T2) & Lo (T1);

      IF X >= 0
      THEN
         IF Y >= 0
         THEN
            RETURN To_Pos_Int (T2);
         ELSE
            RETURN To_Neg_Int (T2);
         END IF;
      ELSE -- X < 0
         IF Y < 0
         THEN
            RETURN To_Pos_Int (T2);
         ELSE
            RETURN To_Neg_Int (T2);
         END IF;
      END IF;

   END Multiply_With_Ovflo_Check;

   -----------------
   -- Raise_Error --
   -----------------

   PROCEDURE Raise_Error
   IS
   BEGIN
      RAISE Constraint_Error WITH "64-bit arithmetic overflow";
   END Raise_Error;

   -------------------
   -- Scaled_Divide --
   -------------------

   PROCEDURE Scaled_Divide
      (X, Y, Z :     int64;
       Q, R    : OUT int64;
       Round   :     boolean)
   IS
      Xu  : CONSTANT uns64 := ABS X;
      Xhi : CONSTANT uns32 := Hi (Xu);
      Xlo : CONSTANT uns32 := Lo (Xu);

      Yu  : CONSTANT uns64 := ABS Y;
      Yhi : CONSTANT uns32 := Hi (Yu);
      Ylo : CONSTANT uns32 := Lo (Yu);

      Zu  : uns64 := ABS Z;
      Zhi : uns32 := Hi (Zu);
      Zlo : uns32 := Lo (Zu);

      D : ARRAY (1 .. 4) OF uns32;
      --  The dividend, four digits (D(1) is high order)

      Qd : ARRAY (1 .. 2) OF uns32;
      --  The quotient digits, two digits (Qd(1) is high order)

      S1, S2, S3 : uns32;
      --  Value to subtract, three digits (S1 is high order)

      Qu : uns64;
      Ru : uns64;
      --  Unsigned quotient and remainder

      Scale : natural;
      --  Scaling factor used for multiple-precision divide. Dividend and
      --  Divisor are multiplied by 2 ** Scale, and the final remainder is
      --  divided by the scaling factor. The reason for this scaling is to
      --  allow more accurate estimation of quotient digits.

      T1, T2, T3 : uns64;
      --  Temporary values

   BEGIN
      --  First do the multiplication, giving the four digit dividend

      T1    := Xlo * Ylo;
      D (4) := Lo (T1);
      D (3) := Hi (T1);

      IF Yhi /= 0
      THEN
         T1    := Xlo * Yhi;
         T2    := D (3) + Lo (T1);
         D (3) := Lo (T2);
         D (2) := Hi (T1) + Hi (T2);

         IF Xhi /= 0
         THEN
            T1    := Xhi * Ylo;
            T2    := D (3) + Lo (T1);
            D (3) := Lo (T2);
            T3    := D (2) + Hi (T1);
            T3    := T3 + Hi (T2);
            D (2) := Lo (T3);
            D (1) := Hi (T3);

            T1    := (D (1) & D (2)) + uns64'(Xhi * Yhi);
            D (1) := Hi (T1);
            D (2) := Lo (T1);

         ELSE
            D (1) := 0;
         END IF;

      ELSE
         IF Xhi /= 0
         THEN
            T1    := Xhi * Ylo;
            T2    := D (3) + Lo (T1);
            D (3) := Lo (T2);
            D (2) := Hi (T1) + Hi (T2);

         ELSE
            D (2) := 0;
         END IF;

         D (1) := 0;
      END IF;

      --  Now it is time for the dreaded multiple precision division. First an
      --  easy case, check for the simple case of a one digit divisor.

      IF Zhi = 0
      THEN
         IF D (1) /= 0 OR ELSE D (2) >= Zlo
         THEN
            Raise_Error;

            --  Here we are dividing at most three digits by one digit

         ELSE
            T1 := D (2) & D (3);
            T2 := Lo (T1 REM Zlo) & D (4);

            Qu := Lo (T1 / Zlo) & Lo (T2 / Zlo);
            Ru := T2 REM Zlo;
         END IF;

         --  If divisor is double digit and too large, raise error

      ELSIF (D (1) & D (2)) >= Zu
      THEN
         Raise_Error;

      --  This is the complex case where we definitely have a double digit
      --  divisor and a dividend of at least three digits. We use the classical
      --  multiple division algorithm (see section (4.3.1) of Knuth's "The Art
      --  of Computer Programming", Vol. 2 for a description (algorithm D).

      ELSE
         --  First normalize the divisor so that it has the leading bit on.
         --  We do this by finding the appropriate left shift amount.

         Scale := 0;

         IF (Zhi AND 16#FFFF0000#) = 0
         THEN
            Scale := 16;
            Zu    := Shift_Left (Zu, 16);
         END IF;

         IF (Hi (Zu) AND 16#FF00_0000#) = 0
         THEN
            Scale := Scale + 8;
            Zu    := Shift_Left (Zu, 8);
         END IF;

         IF (Hi (Zu) AND 16#F000_0000#) = 0
         THEN
            Scale := Scale + 4;
            Zu    := Shift_Left (Zu, 4);
         END IF;

         IF (Hi (Zu) AND 16#C000_0000#) = 0
         THEN
            Scale := Scale + 2;
            Zu    := Shift_Left (Zu, 2);
         END IF;

         IF (Hi (Zu) AND 16#8000_0000#) = 0
         THEN
            Scale := Scale + 1;
            Zu    := Shift_Left (Zu, 1);
         END IF;

         Zhi := Hi (Zu);
         Zlo := Lo (Zu);

         --  Note that when we scale up the dividend, it still fits in four
         --  digits, since we already tested for overflow, and scaling does
         --  not change the invariant that (D (1) & D (2)) >= Zu.

         T1    := Shift_Left (D (1) & D (2), Scale);
         D (1) := Hi (T1);
         T2    := Shift_Left (0 & D (3), Scale);
         D (2) := Lo (T1) OR Hi (T2);
         T3    := Shift_Left (0 & D (4), Scale);
         D (3) := Lo (T2) OR Hi (T3);
         D (4) := Lo (T3);

         --  Loop to compute quotient digits, runs twice for Qd(1) and Qd(2)

         FOR J IN 0 .. 1
         LOOP

            --  Compute next quotient digit. We have to divide three digits by
            --  two digits. We estimate the quotient by dividing the leading
            --  two digits by the leading digit. Given the scaling we did above
            --  which ensured the first bit of the divisor is set, this gives
            --  an estimate of the quotient that is at most two too high.

            Qd (J + 1) :=
               (IF D (J + 1) = Zhi THEN 2**32 - 1
                ELSE Lo ((D (J + 1) & D (J + 2)) / Zhi));

            --  Compute amount to subtract

            T1 := Qd (J + 1) * Zlo;
            T2 := Qd (J + 1) * Zhi;
            S3 := Lo (T1);
            T1 := Hi (T1) + Lo (T2);
            S2 := Lo (T1);
            S1 := Hi (T1) + Hi (T2);

            --  Adjust quotient digit if it was too high

            LOOP
               EXIT WHEN Le3 (S1, S2, S3, D (J + 1), D (J + 2), D (J + 3));
               Qd (J + 1) := Qd (J + 1) - 1;
               Sub3 (S1, S2, S3, 0, Zhi, Zlo);
            END LOOP;

            --  Now subtract S1&S2&S3 from D1&D2&D3 ready for next step

            Sub3 (D (J + 1), D (J + 2), D (J + 3), S1, S2, S3);
         END LOOP;

         --  The two quotient digits are now set, and the remainder of the
         --  scaled division is in D3&D4. To get the remainder for the
         --  original unscaled division, we rescale this dividend.

         --  We rescale the divisor as well, to make the proper comparison
         --  for rounding below.

         Qu := Qd (1) & Qd (2);
         Ru := Shift_Right (D (3) & D (4), Scale);
         Zu := Shift_Right (Zu, Scale);
      END IF;

      --  Deal with rounding case

      IF Round AND THEN Ru > (Zu - uns64'(1)) / uns64'(2)
      THEN
         Qu := Qu + uns64 (1);
      END IF;

      --  Set final signs (RM 4.5.5(27-30))

      --  Case of dividend (X * Y) sign positive

      IF (X >= 0 AND THEN Y >= 0) OR ELSE (X < 0 AND THEN Y < 0)
      THEN
         R := To_Pos_Int (Ru);
         Q := (IF Z > 0 THEN To_Pos_Int (Qu) ELSE To_Neg_Int (Qu));

         --  Case of dividend (X * Y) sign negative

      ELSE
         R := To_Neg_Int (Ru);
         Q := (IF Z > 0 THEN To_Neg_Int (Qu) ELSE To_Pos_Int (Qu));
      END IF;
   END Scaled_Divide;

   ----------
   -- Sub3 --
   ----------

   PROCEDURE Sub3
      (X1, X2, X3 : IN OUT uns32;
       Y1, Y2, Y3 :        uns32)
   IS
   BEGIN
      IF Y3 > X3
      THEN
         IF X2 = 0
         THEN
            X1 := X1 - 1;
         END IF;

         X2 := X2 - 1;
      END IF;

      X3 := X3 - Y3;

      IF Y2 > X2
      THEN
         X1 := X1 - 1;
      END IF;

      X2 := X2 - Y2;
      X1 := X1 - Y1;
   END Sub3;

   -------------------------------
   -- Subtract_With_Ovflo_Check --
   -------------------------------

   FUNCTION Subtract_With_Ovflo_Check
      (X, Y : int64)
      RETURN int64
   IS
      R : CONSTANT int64 := To_Int (To_Uns (X) - To_Uns (Y));

   BEGIN
      IF X >= 0
      THEN
         IF Y > 0 OR ELSE R >= 0
         THEN
            RETURN R;
         END IF;

      ELSE -- X < 0
         IF Y <= 0 OR ELSE R < 0
         THEN
            RETURN R;
         END IF;
      END IF;

      Raise_Error;
   END Subtract_With_Ovflo_Check;

   ----------------
   -- To_Neg_Int --
   ----------------

   FUNCTION To_Neg_Int
      (A : uns64)
      RETURN int64
   IS
      R : CONSTANT int64 := (IF A = 2**63 THEN int64'First ELSE -To_Int (A));
      --  Note that we can't just use the expression of the Else, because it
      --  overflows for A = 2**63.
   BEGIN
      IF R <= 0
      THEN
         RETURN R;
      ELSE
         Raise_Error;
      END IF;
   END To_Neg_Int;

   ----------------
   -- To_Pos_Int --
   ----------------

   FUNCTION To_Pos_Int
      (A : uns64)
      RETURN int64
   IS
      R : CONSTANT int64 := To_Int (A);
   BEGIN
      IF R >= 0
      THEN
         RETURN R;
      ELSE
         Raise_Error;
      END IF;
   END To_Pos_Int;

END System.Arith_64;
