--3rd order IIR filter using floating point computation for signed input samples
--Filter is based on decimated samples at rate = SAMPRATE hz.
--We generate a 50% psuedo clk at SAMPRATE, and latch incoming samples after its rising edge.
--Filter output is generated and latched when the computation completes,
--  then avoids a race condition by waiting till the sample clock goes low to
--  return to the IDLE state to wait for the next sample. 
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.FloatPt.all;		--floating point package includes all the FP components

entity LowPassFP3 is
generic(INCLK	: 	real:= 50_000_000.0; --generic input clk and sampling rate in hz
		SAMPRATE:	real:= 43_000.0);	
port(clk		: 	in std_logic;	--50MHz master clock. Samples decimated to SAMPRATE.
	reset		: 	in std_logic;
	vin		: 	in signed(15 downto 0);			--Input Sample, V(i).
	--dbug		: 	out std_logic_vector(31 downto 0);
	--dbug2		: 	out std_logic_vector(31 downto 0);
	vout		: 	out signed(15 downto 0)			--output Sample
	);
end LowPassFP3;

architecture arch of LowPassFP3 is
--------Filter Parameters----------
--Fs = 40000 Hz, Fc = 1000 Hz, Order = 3
--y(n) = b(1)*x(n) + b(2)*x(n-1) + b(3)*x(n-2) + b(4)*x(n-3) - a(1)*y(n-1) - a(2)*y(n-2) - a(3)*y(n-3)	   
--Where the coefficients b and a are defined as:
--b = 0.00041651651654461446139076, 0.001249638417227, 0.001249638417227, 0.000416546139076
--a = -2.686157396548143, 2.419655110966473, -0.730165345305723
--NOTE the ai signs are flipped so y0 is accumulated by a series of ADDs
--Process steps: 
-- x2<=x1, x1<=x0, x0<=signedFP(vin), y3<=y2, y2<= y1, y1<= y0
--	y0 <= b3*x3 (this is actually the old x2. the new x3 is not saved)		
--	y0<=y0+b2*x2  			
--	y0<=y0+b1*x1			
--	y0<=y0+b0*x0  
--	y0<=y0+a3*y3 --signs of a3-a1 are flipped			
--	y0<=y0+a2*y2 			
--	y0<=y0+a1*y1
--	vout<= FP_to_signed(y0);  
-----------------------------------
--- Filter Declarations ----------- 
constant b0 : std_logic_vector := x"39B00000";	-- 0.000335683359
constant b1 : std_logic_vector := x"3A83FFFF";	-- 0.0010070800
constant b2 : std_logic_vector := x"3A83FFFF";	-- 0.0010070800
constant b3 : std_logic_vector := x"39B00000";	-- 0.000335683359
constant a1 : std_logic_vector := x"402D5000";	-- 2.7080078125
constant a2 : std_logic_vector := x"C01D4100";	-- -2.45709228515
constant a3 : std_logic_vector := x"3F3F1400";	-- 0.74639892578

signal x0, x1, x2 : std_logic_vector(31 downto 0);	--floating point delayed values
signal y0, y1, y2, y3 : std_logic_vector(31 downto 0);
------------------------------------
constant DIV: integer:= integer(INCLK/SAMPRATE);	--sample clk division parameter
signal scnt: integer range 0 to 2*DIV;	--decimation counter for sampling clk
signal clk_samp: std_logic;
--signal dbc, dbc_nxt: integer;
---FP registers------
signal numA: std_logic_vector(31 downto 0);	--floating point A argument
signal numB: std_logic_vector(31 downto 0);	--floating point B argument
signal add_result, mul_result: std_logic_vector(31 downto 0);--FP answer from operation
signal go_add, go_mul: std_logic; 		--top level signal to start an FP operation
signal done_add, done_mul: std_logic;	--bottom level signal that operation is finished
--signal overflow: std_logic;		--No overflow signal. We just saturate on overflow.
-- state machine declarations
type FILT_SM is 
(IDLE, CX3, CX2, AX2, CX1, AX1, CX0, AX0, CY3, AY3, CY2, AY2, CY1, AY1, FINI);
signal state: FILT_SM;
attribute INIT: string;
attribute INIT of state: signal is "IDLE";

begin
----Generate the decimation sample clock.
--New filtered sample is generated each period based on vin when clk_samp goes high.
PSCLK:process (clk, reset) is begin --generate a 50% duty cycle sampling clock 
if reset = '1' then
	scnt<= 0;
elsif rising_edge(clk) then  	
	--dbc<=dbc_nxt;	--*****************
	if scnt < DIV/2 then
		scnt<= scnt + 1;
		clk_samp<= '1';
	elsif scnt < DIV then
		scnt<= scnt + 1;
		clk_samp<= '0';
	else scnt<= 0;
	end if;
end if;
end process PSCLK;
--dbc_nxt<= 0 when state = IDLE or state = FINI else dbc + 1;
---------------------
UMULT: FPP_MULT port map	--instantiate FP MULT
(		A			=> numA,
		B			=> numB,
		clk		=> clk,
		reset		=> reset,
		go			=> go_mul,
		done		=> done_mul,
		overflow	=> open,
		result	=> mul_result );
		
UADD_SUB: FPP_ADD_SUB port map	--instantiate AFP DD_SUB
(		A			=> numA,
		B			=> numB,
		clk		=> clk,
		reset		=> reset,
		go			=> go_add,
		done		=> done_add,
		result	=> add_result);
--------------------------

--Input samples latched and computation begins when clk_samp goes high.
--We wait till clk_samp goes low to go back to IDLE state.
FILT: process (clk, clk_samp, reset, state) is begin  
if reset = '1' then
---Filter Initialization 
	x0 <= (others => '0');
	x1 <= (others => '0');
	x2 <= (others => '0');
	--x3 <= (others => '0');
	y0 <= (others => '0');
	y1 <= (others => '0');
	y2 <= (others => '0');
	y3 <= (others => '0');
	state<= IDLE;
elsif falling_edge(clk) then  
	case state is
		when IDLE	=>	 --wait for a new sample, latch it and start computation
			if clk_samp = '1' then 
				--x3<= x2;
				x2<= x1;
				x1<= x0;
				x0<= SIGNED_TO_FP(vin); --new input sample at SAMP hz
				y3<= y2;
				y2<= y1;
				y1<= y0;
				numA<= x2;	--on this clock x2 is the old x2, i.e. the new x3
				numB<= b3;	--start computing the new b3*x3 term (new x3=old x2)
				go_mul<= '1';
				state<= CX3;	--start accumulating a new y0 with a new x3 term
			end if;
		------
		when CX3 =>		--wait for b3*x3 term to be computed
			if done_mul = '1' then
				go_mul<= '0';
				y0 <= mul_result;	 --new y0=b3x3
				numA<= x2;	--start computing new b2x2 (in this state, x2 is the new value) 
				numB<= b2;
				state<= CX2;
			end if;
		------
		when CX2 =>		--wait for x2 product to complete
			go_mul<= '1';
			if done_mul = '1' then
				go_mul<= '0';
				numA<= y0;			--start accumulation of b2x2 term into y0
				numB<= mul_result;
				go_add<= '1';
				state<= AX2;
			end if;
		------
		when AX2 =>		--wait for x2 to be accumulated into y0
			if done_add = '1' then
				go_add<= '0';
				y0 <= add_result; --y0 now = b3x3+b2x2
				numA<= x1;
				numB<= b1;
				go_mul<= '1';	--start computing new b1*x1 term
				state<= CX1;
			end if;
		------
		when CX1 =>		--wait for computation of new x1 to complete
			if done_mul = '1' then
				go_mul<= '0';
				numA<= y0;			--start accumulation of new b1x1 term  into y0
				numB<= mul_result;
				go_add<= '1';
				state<= AX1;
			end if;
		------
		when AX1 =>		--wait for x1 term to be accumulated into y0
			if done_add = '1' then
				go_add<= '0';
				y0 <= add_result;		--y0 now = b3x3+b2x2+b1x1
				numA<= x0;		--x0 is the current sample
				numB<= b0;		--start computing new b0*sample term
				go_mul<= '1';
				state<= CX0;
			end if;
		------
		when CX0 =>		--wait for new x0 to be computed
			if done_mul = '1' then
				go_mul<= '0';	
				numA<= y0;	--start accumulating x0 term into y0
				numB<= mul_result;
				go_add<= '1';
				state<= AX0;
			end if;
		------
		when AX0 =>		--wait for accumulation of new x0 term
			if done_add = '1' then
				go_add<= '0';
				y0 <= add_result;	--now y0=b3x3+b2x2+b1x1+b0x0
				numA<= y3;			--start computing new a3*y3
				numB<= a3;
				go_mul<= '1';
				state<= CY3;
			end if;
		------
		when CY3 =>		--wait for computation of new a3y3 to complete
			if done_mul = '1' then
				go_mul<= '0';
				numA<= y0;			--start accumulation of new y3 term into y0
				numB<= mul_result;
				go_add<= '1';
				state<= AY3;
			end if;
		------
		when AY3 =>		--wait for a3y3 term to be accumulated into y0
			if done_add = '1' then
				go_add<= '0';
				y0 <= add_result;		--now y0 = b3x3+b2x2+b1x1+b0x0+a3y3
				numA<= y2;
				numB<= a2;		--start computing new a2*y2 term
				go_mul<= '1';
				state<= CY2;
			end if;
		------
		when CY2 =>		--wait for computation of new a2y2 to complete
			if done_mul = '1' then
				go_mul<= '0';
				numA<= y0;			--start accumulation of new a2y2 term into y0
				numB<= mul_result;
				go_add<= '1';
				state<= AY2;
			end if;
		------
		when AY2 =>		--wait for a2y2 term to be accumulated into y0
			if done_add = '1' then
				go_add<= '0';
				y0 <= add_result;	--now y0 = b3x3+b2x2+b1x1+b0x0+a3y3+a2y2
				numA<= y1;
				numB<= a1;		--start computing new a1y1 term
				go_mul<= '1';
				state<= CY1;
			end if;
		------
		when CY1 =>		--wait for computation of new y1 term to complete
			if done_mul = '1' then
				go_mul<= '0';
				numA<= y0;			--start accumulation of a1y1 into y0
				numB<= mul_result;
				go_add<= '1';
				state<= AY1;
			end if;
		------
		when AY1 =>		--wait for a1y1 to be accumulated into y0
			if done_add = '1' then
				go_add<= '0';
				y0 <= add_result;	--final y0=b3x3+b2x2+b1x1+b0x0+a3y3+a2y2+a1y1
				vout <= FP_TO_SIGNED(add_result, 16); --convert and latch the new vout
				--vout <= FP_TO_SIGNED(x0,16); --*****debug
				state<= FINI;
			end if;
		------
		when FINI	=>		--wait for samp_clk to go low to avoid a race condition
			if clk_samp = '0' then
				state<= IDLE; 
			end if;
		------	
		when others		=>
			state <= IDLE;
		------	
	end case;
end if;
end process;

END arch;
