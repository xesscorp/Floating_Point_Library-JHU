-------------------------------------------------------
-- Johns Hopkins University - FPGA Senior Projects, R.E.Jenkins
--Floating point vhdl Package - Ryan Fay, Alex Hsieh, David Jeang
--This file contains the components and functions used for Floating point arithmetic
---------
--Copywrite Johns Hopkins University ECE department. This software may be freely
-- used and modified as long as this acknowledgement is retained.
--------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package FloatPt is
--------------------
--convert a signed integer to flt point format
  function SIGNED_TO_FP(v  : signed) return std_logic_vector;
--convert a number in 32-bit flt format to a signed vector of length N
  function FP_TO_SIGNED(fp : std_logic_vector; N : integer) return signed;
---------------------
  component FPP_MULT is
    port(A        : in  std_logic_vector(31 downto 0);
         B        : in  std_logic_vector(31 downto 0);
         clk      : in  std_logic;
         reset    : in  std_logic;
         go       : in  std_logic;
         done     : out std_logic;
         overflow : out std_logic;
         result   : out std_logic_vector(31 downto 0)
         );
  end component;
-----------------------
  component FPP_ADD_SUB is
    port(A      : in  std_logic_vector(31 downto 0);
         B      : in  std_logic_vector(31 downto 0);
         clk    : in  std_logic;
         reset  : in  std_logic;
         go     : in  std_logic;
         done   : out std_logic;
         result : out std_logic_vector(31 downto 0)
         );
  end component;
-----------------------
  component FPP_DIVIDE is
    port (A        : in  std_logic_vector(31 downto 0);  --Dividend
          B        : in  std_logic_vector(31 downto 0);  --Divisor
          clk      : in  std_logic;     --Master clock
          reset    : in  std_logic;     --Global asynch reset
          go       : in  std_logic;     --Enable
          done     : out std_logic;     --Flag for done computing
          overflow : out std_logic;     --Flag for overflow
          result   : out std_logic_vector(31 downto 0)  --Holds final FP result
          );
  end component;
------------------------
  component MantissaDivision is
    generic (NBIT : integer := 24;
             EBIT : integer := 8);
    port(
      clkin : in  std_logic;            --50 mhz expected in
      reset : in  std_logic;  --only needed to initialize state machine 
      start : in  std_logic;            --external start request
      done  : out std_logic;            --division complete signal out
      as    : in  unsigned(NBIT-1 downto 0);  --aligned  dividend mantissa
      bs    : in  unsigned(NBIT-1 downto 0);  --divisor mantissa
      qs    : out unsigned(NBIT-1 downto 0);  -- quotient
      shift : out unsigned(EBIT-1 downto 0)
      );
  end component;
--------------------------
end package FloatPt;

---===========================================
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package body FloatPt is
------------------------
  use IEEE.std_logic_1164.all;
  use IEEE.numeric_std.all;

  function SIGNED_TO_FP(v : signed) return std_logic_vector is
--Convert a signed binary integer to 32-bit floating pt sign-magnitude format 
    variable i   : integer range 0 to v'left+1;
    variable j   : integer range 0 to 255;
    variable fp  : std_logic_vector(31 downto 0);  --returned FP
    variable exp : integer range -1024 to 1024;    --exponent
    variable m   : unsigned(v'length downto 0);    --mantissa + leading bit
    
  begin
    m   := '0' & unsigned(abs(v));  --we use the mag of v to create a mantissa
    --start with biased exp equiv to 2**(LENGTH-1), so m becomes the mantissa, m.mmmmm...
    --i.e. mag(v) = 2**(exp-127) * m.m m m m m....
    exp := 127 + m'length-1;
    --normalize m as the mantissa with one bit in front of the decimal point 
    for i in 0 to m'left loop
      if m(m'left) = '1' then
        j := i;
        exit;
      else
        m := m(m'left-1 downto 0) & '0';
                                        --exp:= exp - 1;
      end if;
    end loop;
    exp              := exp - j;
    fp(30 downto 23) := std_logic_vector(TO_UNSIGNED(exp, 8));
    --Make sure we have enough bits for a normalized full mantissa (23)
    -- and at the same time remove the mantissa leading 1
    if m'length < 24 then  -- <24 bits, bottom bits set to 0, and drop the leading 1        
      fp(23-m'length downto 0)  := (others => '0');
      fp(22 downto 24-m'length) := std_logic_vector(m(m'length-2 downto 0));
    else  --if >= 24, drop leading 1 and take next 23 bits for fp
      fp(22 downto 0) := std_logic_vector(m(m'length-2 downto m'length-24));
    end if;

    if v(v'left) = '1' then
      fp(31) := '1';
    else fp(31) := '0';
    end if;
    return fp;
  end function SIGNED_TO_FP;
--------------------------------
  use IEEE.std_logic_1164.all;
  use IEEE.numeric_std.all;
--Convert a number in std 32-bit flt pt format to a signed binary integer with N bits.
--NOTE that N must be large enough for the entire truncated result as a signed integer
--If the number is positive, the result can be typecast into unsigned if desired.
  function FP_TO_SIGNED(fp : std_logic_vector; N : integer) return signed is
    variable num    : unsigned(N+1 downto 0);
    variable result : signed(N+1 downto 0);   --returned number
    variable exp    : integer range -1024 to 1023;
    variable m      : unsigned(24 downto 0);
  begin
    m   := "01" & unsigned(fp(22 downto 0));  --restore the mantissa leading 1 
    exp := TO_INTEGER(unsigned(fp(30 downto 23))) - 127;  --unbias the exponent
    if exp < 0 then                     --number less than 1 truncated to 0
      num := (others => '0');
    elsif exp >= N then
      num := (others => '1');           --num greater than 2**N saturates
    else
      num(exp+1 downto 0)   := m(24 downto 23-exp);  --effectively multiply m by 2**exp,
      num(N+1 downto exp+2) := (others => '0');  --  and pad with leading 0's.
    end if;
    if fp(31) = '1' then
      result := -signed(num);
    else
      result := signed(num);
    end if;
    return result(N-1 downto 0);
  end function FP_TO_SIGNED;
---------------------------------
end package body FloatPt;
---================================================

--******************************************************
--Module to Produce a 32-bit floating point product from 2 operands - Ryan Fay
--We presume a mult. request starts the process, and a completion signal is generated.
-- This generally takes 4 clocks to complete, if overflow is checked for.
--******************************************************
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity FPP_MULT is
  port(A        : in  std_logic_vector(31 downto 0);  --input operands
       B        : in  std_logic_vector(31 downto 0);
       clk      : in  std_logic;
       reset    : in  std_logic;
       go       : in  std_logic;
       done     : out std_logic;
       overflow : out std_logic;
       result   : out std_logic_vector(31 downto 0)
       );
end FPP_MULT;
-----------------------------------
architecture ARCH of FPP_MULT is

  constant BIAS : unsigned(8 downto 0) := to_unsigned(127, 9);  --exponent bias of 127

  signal full_mantissa : std_logic_vector(47 downto 0);
  signal full_exp      : std_logic_vector(8 downto 0);  --extra msb for unsigned exponent addition
  signal R_sign        : std_logic;

  signal Aop, Bop : std_logic_vector(31 downto 0);  --latched operands
  alias A_sign    : std_logic is Aop(31);           --operand segments
  alias A_exp     : std_logic_vector(7 downto 0) is Aop(30 downto 23);
  alias A_man     : std_logic_vector(22 downto 0) is Aop(22 downto 0);

  alias B_sign : std_logic is Bop(31);
  alias B_exp  : std_logic_vector(7 downto 0) is Bop(30 downto 23);
  alias B_man  : std_logic_vector(22 downto 0) is Bop(22 downto 0);

-- STATE MACHINE DECLARATION
  type MULT_SM is (WAITM, MAN_EXP, CHECK, NORMALIZE, PAUSE);
  signal MULTIPLY            : MULT_SM;
  attribute INIT             : string;
  attribute INIT of MULTIPLY : signal is "WAITM";
-----------------------------------
begin

  process (MULTIPLY, clk, go, reset) is
  begin
    if (reset = '1') then
      full_mantissa <= (others => '0');
      full_exp      <= (others => '0');
      done          <= '0';
      MULTIPLY      <= WAITM;
    elsif (rising_edge(clk)) then
      case (MULTIPLY) is
        when WAITM =>
          overflow <= '0';
          done     <= '0';
          if (go = '1') then
            Aop      <= A;              --latch input values
            Bop      <= B;
            MULTIPLY <= MAN_EXP;
          else
            MULTIPLY <= WAITM;
          end if;
        when MAN_EXP =>  --compute a sign, exponent and matissa for product
          R_sign        <= A_sign xor B_sign;
          full_mantissa <= std_logic_vector(unsigned('1' & A_man) * unsigned('1' & B_man));
          --full_exp <= std_logic_vector( (unsigned(A_exp)-BIAS) + (unsigned(B_exp)-BIAS)+ BIAS );
          full_exp      <= std_logic_vector((unsigned(A_exp)- BIAS) + unsigned(B_exp));
                                        --MULTIPLY <= CHECK;
          MULTIPLY      <= NORMALIZE;
        when CHECK =>                   --Check for exponent overflow
          if (unsigned(full_exp) > 255) then
            overflow <= '1';
            result   <= (31 => R_sign, others => '1');
            done     <= '1';
            MULTIPLY <= PAUSE;
          else
            MULTIPLY <= NORMALIZE;
          end if;
        when NORMALIZE =>
          if full_mantissa(47) = '1' then
            full_mantissa <= '0' & full_mantissa(47 downto 1);
            full_exp      <= std_logic_vector(unsigned(full_exp) + 1);
          else
            result   <= R_sign & full_exp(7 downto 0) & full_mantissa(45 downto 23);
            done     <= '1';            --signal that operation completed
            MULTIPLY <= PAUSE;
          end if;
        when PAUSE =>                   -- wait for acknowledgement
          if (go = '0') then
            done     <= '0';
            MULTIPLY <= WAITM;
          end if;
        when others =>
          MULTIPLY <= WAITM;
      end case;
    end if;
  end process;

end ARCH;
---===============================================================
--Module for a signed floating point add - David Jeang. Thanks to Prof. Jenkins
--We presume an ADD request starts the process, and a completion signal is generated.
--Subtraction is accomplished by negating the B input, before requesting the add.
--The add can easily exceed 15 clocks, depending on the exponent difference
--   and post-normalization.
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity FPP_ADD_SUB is
  port(A      : in  std_logic_vector(31 downto 0);
       B      : in  std_logic_vector(31 downto 0);
       clk    : in  std_logic;
       reset  : in  std_logic;
       go     : in  std_logic;
       done   : out std_logic;
       result : out std_logic_vector(31 downto 0)
       );
end FPP_ADD_SUB;

architecture Arch of FPP_ADD_SUB is
  type SMC is (WAITC, ALIGN, ADDC, NORMC, PAUSEC);
  signal ADD_SUB            : SMC;
  attribute INIT            : string;
  attribute INIT of ADD_SUB : signal is "WAITC";  --we  powerup in WAITC state

---signals latched from inputs
  signal A_mantissa, B_mantissa : std_logic_vector (24 downto 0);
  signal A_exp, B_exp           : std_logic_vector (8 downto 0);
  signal A_sgn, B_sgn           : std_logic;
--output signals
  signal sum                    : std_logic_vector (31 downto 0);
--signal F_exp: std_logic_vector (7 downto 0);
  signal mantissa_sum           : std_logic_vector (24 downto 0);

begin

  result <= sum;                        --sum is built in the state machine
--State machine to wait for request, latch inputs, align exponents, perform add, normalize result
  SM : process (clk, reset, ADD_SUB, go) is
    variable diff : signed(8 downto 0);
--variable j: integer range 0 to 31;
  begin
    if(reset = '1') then
      ADD_SUB <= WAITC;                 --start in wait state
      done    <= '0';
    elsif rising_edge(clk) then
      case ADD_SUB is
        when WAITC =>
          if (go = '1') then            --wait till start request goes high
            A_sgn      <= A(31);
            B_sgn      <= B(31);
            A_exp      <= '0' & A(30 downto 23);
            B_exp      <= '0' & B(30 downto 23);
            A_mantissa <= "01" & A(22 downto 0);
            B_mantissa <= "01" & B(22 downto 0);
            ADD_SUB    <= ALIGN;
          else
            ADD_SUB <= WAITC;           --not needed, but clearer
          end if;
        when ALIGN =>  --exponent alignment. Always makes A_exp be final exponent
          --Below method is like a barrel shift, but is big space hog------
          --note that if either num is greater by 2**24, we skip the addition.
          if unsigned(A_exp) = unsigned(B_exp) then
            ADD_SUB <= ADDC;
          elsif unsigned(A_exp) > unsigned(B_exp) then
            diff := signed(A_exp) - signed(B_exp);  --B needs downshifting
            if diff > 23 then
              mantissa_sum <= A_mantissa;  --B insignificant relative to A
              sum(31)      <= A_sgn;
              ADD_SUB      <= PAUSEC;   --go latch A as output
            else       --downshift B to equilabrate B_exp to A_exp
              B_mantissa(24-TO_INTEGER(diff) downto 0)  <= B_mantissa(24 downto TO_INTEGER(diff));
              B_mantissa(24 downto 25-TO_INTEGER(diff)) <= (others => '0');
              ADD_SUB                                   <= ADDC;
            end if;
          else                          --A_exp < B_exp. A needs downshifting
            diff := signed(B_exp) - signed(A_exp);
            if diff > 23 then
              mantissa_sum <= B_mantissa;  --A insignificant relative to B
              sum(31)      <= B_sgn;
              A_exp        <= B_exp;  --this is just a hack since A_exp is used for final result
              ADD_SUB      <= PAUSEC;   --go latch B as output
            else       --downshift A to equilabrate A_exp to B_exp
              A_exp                                     <= B_exp;
              A_mantissa(24-TO_INTEGER(diff) downto 0)  <= A_mantissa(24 downto TO_INTEGER(diff));
              A_mantissa(24 downto 25-TO_INTEGER(diff)) <= (others => '0');
              ADD_SUB                                   <= ADDC;
            end if;
          end if;
        ---------------------------------------
        --This way iterates the alignment shifts, but is way too slow
        -- If either num is greater by 2**23, we just skip the addition.
--                              if (signed(A_exp) - signed(B_exp))> 23 then
--                                      mantissa_sum <= std_logic_vector(unsigned(A_mantissa));
--                                      sum(31) <= A_sgn;
--                                      ADD_SUB <= PAUSEC;  --go latch A as output
--                              elsif (signed(B_exp) - signed(A_exp))> 23 then
--                                      mantissa_sum <= std_logic_vector(unsigned(B_mantissa));
--                                      sum(31) <= B_sgn;
--                                      A_exp <= B_exp;
--                                      ADD_SUB <= PAUSEC;  --go latch B as output.
--                              --otherwise we normalize the smaller exponent to the larger.
--                              elsif(unsigned(A_exp) < unsigned(B_exp)) then
--                                      A_mantissa <= '0' & A_mantissa(24 downto 1);
--                                      A_exp <= std_logic_vector((unsigned(A_exp)+1));
--                              elsif (unsigned(B_exp) < unsigned(A_exp)) then
--                                      B_mantissa <= '0' & B_mantissa(24 downto 1);
--                                      B_exp <= std_logic_vector((unsigned(B_exp)+1));
--                              else
--                                      --either way, A_exp is the final equilabrated exponent
--                                      ADD_SUB <= ADDC;
--                              end if;
        -----------------------------------------
        when ADDC =>                    --Mantissa addition
          ADD_SUB <= NORMC;
          if (A_sgn xor B_sgn) = '0' then  --signs are the same. Just add 'em
            mantissa_sum <= std_logic_vector((unsigned(A_mantissa) + unsigned(B_mantissa)));
            sum(31)      <= A_sgn;      --both nums have same sign
          --otherwise subtract smaller from larger and use sign of larger
          elsif unsigned(A_mantissa) >= unsigned(B_mantissa) then
            mantissa_sum <= std_logic_vector((unsigned(A_mantissa) - unsigned(B_mantissa)));
            sum(31)      <= A_sgn;
          else
            mantissa_sum <= std_logic_vector((unsigned(B_mantissa) - unsigned(A_mantissa)));
            sum(31)      <= B_sgn;
          end if;

        when NORMC =>  --post normalization. A_exp is the exponent of the unormalized sum
          if unsigned(mantissa_sum) = TO_UNSIGNED(0, 25) then
            mantissa_sum <= (others => '0');  --break out if a mantissa of 0
            A_exp        <= (others => '0');
            ADD_SUB      <= PAUSEC;     --
          elsif(mantissa_sum(24) = '1') then  --if sum overflowed we downshift and are done.
            mantissa_sum <= '0' & mantissa_sum(24 downto 1);  --shift the 1 down
            A_exp        <= std_logic_vector((unsigned(A_exp)+ 1));
            ADD_SUB      <= PAUSEC;
          elsif(mantissa_sum(23) = '0') then  --in this case we need to upshift
            --Below takes big resources to determine the normalization upshift, 
            --  but does it one step.
            for i in 22 downto 1 loop   --find position of the leading 1
              if mantissa_sum(i) = '1' then
                mantissa_sum(24 downto 23-i) <= mantissa_sum(i+1 downto 0);
                mantissa_sum(22-i downto 0)  <= (others => '0');  --size of shift= 23-i
                A_exp                        <= std_logic_vector(unsigned(A_exp)- 23 + i);
                exit;
              end if;
            end loop;
            ADD_SUB <= PAUSEC;          --go latch output, wait for acknowledge
                                        ------------------------------
          --This iterates the normalization shifts, thus can take many clocks.
          --mantissa_sum <= mantissa_sum(23 downto 0) & '0';
          --A_exp <= std_logic_vector((unsigned(A_exp)-1));
          --ADD_SUB<= NORMC; --keep shifting till  leading 1 appears
          ------------------------------
          else
            ADD_SUB <= PAUSEC;  --leading 1 already there. Latch output, wait for acknowledge
          end if;
        when PAUSEC =>
          sum(22 downto 0)  <= mantissa_sum(22 downto 0);
          sum(30 downto 23) <= A_exp(7 downto 0);
          done              <= '1';     -- signal done
          if (go = '0') then            --pause till request ends
            done    <= '0';
            ADD_SUB <= WAITC;
          end if;
        when others => ADD_SUB <= WAITC;      --Just in case.
      end case;
    end if;
  end process SM;

end Arch;
--=================================================================
---*****************************************
-- Floating point divide control module
-- Doanwhey Alexander Hsieh. Special thanks to Prof. Robert E. Jenkins  
--******************************************
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
use WORK.FloatPt.all;
---Uncomment the following library declaration if instantiating any Xilinx primitives.
---library UNISIM;
---use UNISIM.VComponents.all;

entity FPP_DIVIDE is
  port (A        : in  std_logic_vector(31 downto 0);  --Dividend
        B        : in  std_logic_vector(31 downto 0);  --Divisor
        clk      : in  std_logic;       --Master clock
        reset    : in  std_logic;       --Global asynch reset
        go       : in  std_logic;       --Enable
        done     : out std_logic;       --Flag for done computing
        --ZD:         out std_logic;                                          --Flag for zero divisor
        overflow : out std_logic;       --Flag for overflow
        result   : out std_logic_vector(31 downto 0)   --Holds final FP result
        );
end FPP_DIVIDE;

architecture Arch of FPP_DIVIDE is
--=======Constants===========
  constant ZERO_FP : std_logic_vector(30 downto 0) := (others => '0');
  constant EBIAS   : unsigned(8 downto 0)          := to_unsigned(127, 9);  --exponent bias
--=======Floating Point Divide State Machine===
  type FPDiv is (FPDivIdle,             --Waits for GO signal to be set
                 FPDivAlign,            --Align dividend
                 FPDivSetExponent,      --Set the quotient exponent
                 FPDivStart,            --start mantissa divider
                 FPDivWaitTilDone,      --wait for completion
                 FPDivDone,             --Done with calculation
                 FPDivPause             --Wait for GO signal to be cleared
                 );
  signal FPD            : FPDiv;
  attribute init        : string;
  attribute init of FPD : signal is "FPDivIdle";

--==== Hardware/Registers=====
  signal Ae                : unsigned(8 downto 0);  --Dividend exponent
  signal Be                : unsigned(8 downto 0);  --Divisor exponent
--Quotient parts 
  signal Qs                : std_logic;
  signal Qm                : std_logic_vector(22 downto 0);  --final mantissa with leading 1 gone
  signal Qe                : std_logic_vector(8 downto 0);
--unsigned actual Mantissa of Dividend and Divisor with leading 1 restored 
  signal rdAm, rdBm        : unsigned(23 downto 0);
--normalized mantissa Quotient with leading 1
  signal rdQm              : unsigned(23 downto 0);
--Required exponent reduction from quotient normalization
  signal expShift          : unsigned(7 downto 0);
--===== Clock Signals=====
  signal fpClk0            : std_logic := '0';
  signal fpClk             : std_logic := '0';  --25MHz Clock for DIV state machine
--=====  Miscellaneous ===
  signal restoringDivStart : std_logic;
  signal restoringDivDone  : std_logic;
----------------------------------
begin
------------------------------------------
-- Divided Clock to Drive Floating Point steps
  process (CLK) is
  begin
    if (rising_edge(CLK)) then
      fpClk0 <= not fpClk0;
    end if;
  end process;

  process (fpClk0) is
  begin
    if (rising_edge(fpClk0)) then
      fpClk <= not fpClk;
    end if;
  end process;
---------------------------------------
  UDIV : MantissaDivision  --instantiate module for mantissa division
    generic map(NBIT => 24, EBIT => 8)
    port map(clkin => fpClk,
             reset => reset,
             start => restoringDivStart,
             done  => restoringDivDone,
             as    => rdAm,  --full mantissas with hidden leading 1 restored
             bs    => rdBm,
             qs    => rdQm,
             shift => expShift
             );              
---------------------------------------
-- State Machine for Division Control
----------------------------------------
  process (fpClk, reset) is             --, FPD, GO, A, B
  begin
    if (reset = '1') then
      FPD               <= FPDivIdle;
      done              <= '0';
      overflow          <= '0';
      restoringDivStart <= '0';
    elsif (rising_edge(fpClk)) then
      case FPD is
                                        ------------------------------------
                           -- Wait for GO signal, then begin the FPP division algorithm. 
                           -- If divisor is zero, return all 1's.
                           -- If dividend is zero, return zero. 
                           ------------------------------------
        when FPDivIdle =>
          restoringDivStart <= '0';
          done              <= '0';
          if (go = '1') then
            Qs <= A(31) xor B(31);      --Set sign of quotient
            if (B(30 downto 0) = ZERO_FP) then
              Qm       <= (others => '1');  --Divide by zero, return Max number
              Qe       <= (others => '1');
              overflow <= '1';    --we make no distinction on cause of overflow
              FPD      <= FPDivDone;    --go to done
            elsif (A(30 downto 0) = ZERO_FP) then
              Qm  <= (others => '0');   --Zero dividend, return zero
              Qe  <= (others => '0');
              FPD <= FPDivDone;         --go to done
            else                        --initialize internal registers
              rdAm <= unsigned('1' & A(22 downto 0));  --Actual normalized mantissas
              rdBm <= unsigned('1' & B(22 downto 0));
              Ae   <= unsigned('0' & A(30 downto 23));  --biased exponents with extra msb
              Be   <= unsigned('0' & B(30 downto 23));
              FPD  <= FPDivAlign;
            end if;
          else
            FPD <= FPDivIdle;           --continue waiting
          end if;
                                        ----------
        when FPDivAlign =>   -- Check mantissas and align if Am greater than Bm
          FPD <= FPDivSetExponent;      --default next state
          if rdAm > rdBm then
            rdAm <= '0' & rdAm(23 downto 1);  --downshift to make Am less than Bm
                                              --if Ae < 255 then
            Ae   <= Ae + 1;
                                        --else 
                           -- Qreg                    <= A(31) & NAN; --Exponent overflow, return NaN
                           -- overflow                <= '1'; --we make no distinction on cause of overflow
                           -- FPD                     <= FPDivDone;  --go to Calculation done
                           --end if;  
          end if;
                                        ---------
                           --Maybe we should break the exponent subtract into two pieces for speed
        when FPDivSetExponent =>
          if Ae > Be then
            Qe <= std_logic_vector(unsigned(Ae) - unsigned(Be) + EBIAS);
          else
            Qe <= std_logic_vector(EBIAS - (unsigned(Be) - unsigned(Ae)));
          end if;
          FPD <= FPDivStart;
                                        -----------  
        when FPDivStart =>              --Start the mantissa division
          restoringDivStart <= '1';
          FPD               <= FPDivWaitTilDone;  --Wait for mantissa division to complete
                                                  ----------   
        when FPDivWaitTilDone =>  --Latch normalized mantissa quotient, and new exponent
          if (restoringDivDone = '1') then
            Qe  <= std_logic_vector(unsigned(Qe) - expShift);
            Qm  <= std_logic_vector(rdQm(22 downto 0));  --drop the mantissa leading 1
            FPD <= FPDivDone;
          end if;
                                        ---------
        when FPDivDone =>  --Paste together and latch the final result,  signal done.
          done              <= '1';
          result            <= Qs & Qe(7 downto 0) & Qm;
          restoringDivStart <= '0';
          FPD               <= FPDivPause;
                                        ----------
        when FPDivPause =>   --Pause for the done signal to be recognized
          if (go = '0') then      --request should reset after done goes high
            done <= '0';
            fpd  <= FPDivIdle;
          end if;
                                        ---------            
        when others =>                  --  Default state is FPDivIdle
          FPD <= FPDivIdle;
      end case;
    end if;
  end process;
  
end Arch;

--***********************************************
--Module to Divide an N-bit unsigned mantissa by an N-bit unsigned mantissa to produce
--an N-bit unsigned, normalized Quotient - Alex Hsieh
--We presume a mantissa divide request starts the state machine, and a completion signal is generated. 
--***********************************************
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity MantissaDivision is  -- XSA board driver usb dram operations.
  generic (NBIT : integer := 24;
           EBIT : integer := 8);
  port(
    clkin : in  std_logic;              --50 mhz expected in
    reset : in  std_logic;  --only needed to initialize state machine 
    start : in  std_logic;              --external start request
    done  : out std_logic;              --division complete signal out
    as    : in  unsigned(NBIT-1 downto 0);  --aligned dividend
    bs    : in  unsigned(NBIT-1 downto 0);  --divisor
    qs    : out unsigned(NBIT-1 downto 0);  --normalized quotient with leading 1 supressed
    shift : out unsigned(EBIT-1 downto 0)
    );
end MantissaDivision;

architecture arch of MantissaDivision is
---state machine signals---
  type SM_type is (IDLE, NORM, TEST, CHKBO, FINI);
  signal state                  : SM_type;  --division process state machine
  attribute INIT                : string;
  attribute INIT of state       : signal is "IDLE";  --we powerup in IDLE state
  signal sm_clk                 : std_logic;
--- registers-------------
  signal acc                    : unsigned(NBIT-1 downto 0);  --accumulated quotient
  signal numerator, denominator : unsigned(2*NBIT-1 downto 0);
  signal diff                   : unsigned(2*NBIT-1 downto 0);  --difference between current num. and denom.
  signal shift_I                : unsigned(EBIT-1 downto 0);  --reduction of final exponent for normalization
  signal count                  : integer range 0 to NBIT;  --iteration count.
--- SR inference----
  attribute shreg_extract       : string;   --Don't infer primitive SR's
--attribute shreg_extract of acc: signal is "no";               --To avoid reset problems.
--attribute shreg_extract of numerator: signal is "no"; 

begin
  shift  <= shift_I;
----- division state machine--------------
  diff   <= numerator - denominator;
  sm_clk <= clkin;
  MDIV : process (sm_clk, reset, state, start, as, bs) is
  begin
    if reset = '1' then
      state <= IDLE;  --reset into the idle state to wait for a memory operation.
      done  <= '0';
      
    elsif rising_edge(sm_clk) then      --
      case state is
        when IDLE =>  --we remain in idle till we get a start signal.
          if start = '1' then
            acc                               <= (others => '0');
            numerator(NBIT-1 downto 0)        <= as;
            numerator(2*NBIT-1 downto NBIT)   <= (others => '0');
            denominator(NBIT-1 downto 0)      <= bs;
            denominator(2*NBIT-1 downto NBIT) <= (others => '0');
            count                             <= 0;
            state                             <= TEST;
            done                              <= '0';
            shift_I                           <= (others => '0');
          end if;
                                        -------
        when TEST =>  -- Test, shift, and apply subtraction to numerator if necessary.
          if numerator < denominator then
            acc       <= acc(NBIT-2 downto 0) & '0';
            numerator <= numerator(2*NBIT-2 downto 0) & '0';
          else
            acc       <= acc(NBIT-2 downto 0) & '1';  --next quotient bit is a 1
            numerator <= diff(2*NBIT-2 downto 0) & '0';  --diff = numerator - denominator;
          end if;
          state <= CHKBO;
                                        --------
        when CHKBO =>  --check count for breakout. (this conveniently creates a 1-clk delay)
          if count < NBIT-1 then
            count <= count + 1;
            state <= TEST;
          else
            state <= NORM;
          end if;
                                        ---------
        when NORM =>  --normalize the 24-bit accumulated quotient
          if (acc(NBIT-1) = '0') then
            acc     <= acc(NBIT-2 downto 0) & '0';
            shift_I <= shift_I + 1;
            state   <= NORM;
          else
            qs    <= acc;  --latch normalized quotient for output. Leading bit will be dropped
            done  <= '1';
            state <= FINI;
          end if;
                                        ---------
        when FINI =>  --to avoid a race condition, we wait till start goes off
          if start = '0' then  --this means the upper entity has latched the answer
            state <= IDLE;              --Go wait for next request
            done  <= '0';
          end if;
                                        ---------  
        when others => state <= IDLE;
      end case;
    end if;
  end process MDIV;
---------------------------------
end arch;
--========================================================


