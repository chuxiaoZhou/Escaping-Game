LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY vhdl_game_vga IS

	GENERIC(
		h_pulse 	:	INTEGER   := 96;    	--sync pulse width of horiztonal in pixels
		h_bp	 	:	INTEGER   := 48;		--back porch width of horiztonal in pixels
		h_pixels	:	INTEGER   := 640;		--horiztonal display width in pixels
		h_fp	 	:	INTEGER   := 16;		--horiztonal front porch width in pixels
		h_pol		:	STD_LOGIC := '0';		--horizontal sync pulse polarity (1 = positive, 0 = negative)
		v_pulse 	:	INTEGER   := 2;			--vertical sync pulse width in rows
		v_bp	 	:	INTEGER   := 33;			--vertical back porch width in rows
		v_pixels	:	INTEGER   := 480;		--vertical display width in rows
		v_fp	 	:	INTEGER   := 10;			--vertical front porch width in rows
		v_pol		:	STD_LOGIC := '1');	--vertical sync pulse polarity (1 = positive, 0 = negative)
	PORT(
		clk      	:	IN		STD_LOGIC;	--pixel clock at frequeuency of VGA mode being used
		reset_n		:	IN		STD_LOGIC;	--active low asycnchronous reset
		button_j    :  in    std_LOGIC;  --controler
		h_sync		:	OUT	STD_LOGIC;	--horiztonal sync pulse
		v_sync		:	OUT	STD_LOGIC;	--vertical sync pulse
		n_blank		:	OUT	STD_LOGIC;	--direct blacking output to VGA
		n_sync		:	OUT	STD_LOGIC; --sync-on-green output to VGA
		red			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --red magnitude output to VGA
		green			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --green magnitude output to VGA
		blue			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0'); --blue magnitude output to VGA
		clk_vga     :  out   std_logic; --vga clk 25MHz
		rw, rs, e   :  OUT   STD_LOGIC; --read/write, setup/data, and enable for lcd
		lcd_data    :  OUT   STD_LOGIC_VECTOR(7 DOWNTO 0); --data signals for lcd
		lcd_on      :  OUT   std_logic; --LCD Power ON/OFF
		lcd_blon    :  OUT   std_logic); --LCD Back Light ON/OFF
END vhdl_game_vga;

ARCHITECTURE behavior OF vhdl_game_vga IS
    --states machine for lcd 
	TYPE CONTROL IS(power_up, initialize, ready, send);
	SIGNAL states : CONTROL;
	CONSTANT freque : INTEGER := 50; --system clock frequeuency in MHz
	signal reset_all: std_LOGIC;     --lcd reset
	signal lcd_abled:std_LOGIC;
	signal lcd_bsy:STD_LOGIC_VECTOR(9 DOWNTO 0);
	signal busy, lcd_bsyy : std_LOGIC:='1';
	signal lcd_clk : std_LOGIC;signal disp_ena   :std_LOGIC;
	--vga sync
	signal column     :integer;
	signal row        :integer;
	signal pixel_clk  :  std_logic;
	CONSTANT	horizon_period	:	INTEGER := h_pulse + h_bp + h_pixels + h_fp;  --total number of pixel clocks in a row
	CONSTANT	verticl_period	:	INTEGER := v_pulse + v_bp + v_pixels + v_fp;  --total number of rows in column
--
--
--
--game_Over
constant game_Over_l:integer :=0;--length between picture we need show and left of screen when gameover
constant game_Over_t:integer :=0;--length between picture we need show and top of screen when gameover
constant game_Over_k:integer :=480;--game_Over thickness(total screen)
signal lose_on, lose_on_next :std_logic:='0';
signal over_on, over_on_next :std_logic:='0';
signal game_Over_on   :std_logic;
signal red_game_Over  :std_logic_vector(7 downto 0); 
signal blue_game_Over :std_logic_vector(7 downto 0); 
signal green_game_Over:std_logic_vector(7 downto 0); 

--character_conable
signal character_conable_t,character_conable_t_next:integer :=420; -- length  between top of screen and character
constant character_conable_l:integer :=100;--length between left of screen and character
constant character_conable_k:integer :=20;--character thickness
constant character_conable_w:integer:=20;--character width
constant character_conable_v:integer:=10;--horizontal velocity of the character
signal character_conable_on   :std_logic;
signal red_character_conable  :std_logic_vector(7 downto 0); 
signal green_character_conable:std_logic_vector(7 downto 0); 
signal blue_character_conable :std_logic_vector(7 downto 0); 

--obstruction
--1
signal obstruction1_l,obstruction1_og,obstruction1_l_next:integer :=600;--length between obstruction and left of screen
constant obstruction1_t:integer :=420; --length between obstruction and top of screen
constant obstruction1_h:integer :=40;--obstruction Height
constant obstruction1_w:integer :=40;--obstruction width
constant obstruction1_v:integer:=3;-- horizontal velocity of the obstruction 
signal obstruction1_on :std_logic;
signal green_obstruction1:std_logic_vector(7 downto 0); 
signal blue_obstruction1 :std_logic_vector(7 downto 0); 
signal red_obstruction1  :std_logic_vector(7 downto 0); 

--2
signal obstruction2_l,obstruction2_og,obstruction2_l_next:integer :=625;--length between obstruction and left of screen
constant obstruction2_t:integer :=300; --length between obstruction and top of screen
constant obstruction2_h:integer :=15;--obstruction Height
constant obstruction2_w:integer :=15;--obstruction width
constant obstruction2_v:integer:=2;-- horizontal velocity of the obstruction 
signal obstruction2_on :std_logic;
signal green_obstruction2:std_logic_vector(7 downto 0); 
signal blue_obstruction2 :std_logic_vector(7 downto 0); 
signal red_obstruction2  :std_logic_vector(7 downto 0); 

--3
signal obstruction3_l,obstruction3_og,obstruction3_l_next:integer :=610;--length between obstruction and left of screen
constant obstruction3_t:integer :=100; --length between obstruction and top of screen
constant obstruction3_h:integer :=30;--obstruction Height
constant obstruction3_w:integer :=30;--obstruction width
constant obstruction3_v:integer:=4;-- horizontal velocity of the obstruction 
signal obstruction3_on :std_logic;
signal green_obstruction3:std_logic_vector(7 downto 0); 
signal blue_obstruction3 :std_logic_vector(7 downto 0); 
signal red_obstruction3  :std_logic_vector(7 downto 0); 


--reconditioning(1/60) of 640*480 screen 
signal recondition_regul,recondition_next:integer;
constant recondition_constant:integer:=833333;
signal recondition_tick:std_logic;

--obstruction animation
signal obstruction1_v_regul,obstruction1_v_next:integer:=3;--variable of the horizontal velocity
signal obstruction2_v_regul,obstruction2_v_next:integer:=2;
signal obstruction3_v_regul,obstruction3_v_next:integer:=4;


--x,y pixel cursor
signal x,y:integer range 0 to 650;

--mux
signal mux:std_logic_vector(5 downto 0);
	
--color recondition
signal red_regul,red_next     :std_logic_vector(7 downto 0);
signal green_regul,green_next :std_logic_vector(7 downto 0);	
signal blue_regul,blue_next   :std_logic_vector(7 downto 0);
	
	BEGIN
------------------LCD DISPLAY-----------
----------
--Part one
----------
 lcd_on <= '1'; --LCD Power ON
 lcd_blon<='1'; --LCD Back Light ON

 PROCESS(clk)
VARIABLE clk_count : INTEGER := 0; --event counter for timing
	BEGIN
	IF(clk'EVENT and clk = '1') THEN

		CASE states IS

 --wait 50 ms to ensure Vdd has risen and required LCD wait is met
WHEN power_up =>
busy <= '1';
 IF(clk_count < (50000 * freque)) THEN --wait 50 ms
 clk_count := clk_count + 1;
 states <= power_up;
 ELSE --power-up complete
 clk_count := 0;
 rs <= '0';
 rw <= '0';
 lcd_data <= "00110000"; -- Function Set: 1-line mode,display off lcd_data <= "00110000";
 states <= initialize;
 END IF;

 --cycle through initialization sequence
 WHEN initialize =>
 busy <= '1';
 clk_count := clk_count + 1;
 IF(clk_count < (10 * freque)) THEN --function set
 -- lcd_data <= "00111100"; --2-line mode, display on
 lcd_data <= "00110100"; --1-line mode, display on
 --lcd_data <= "00110000"; --1-line mdoe, display off
 --lcd_data <= "00111000"; --2-line mode, display off
 e <= '1';
 states <= initialize;
 ELSIF(clk_count < (60 * freque)) THEN --wait 50 us
 lcd_data <= "00000000";
 e <= '0';
 states <= initialize;
 ELSIF(clk_count < (70 * freque)) THEN --display on/off control
 --lcd_data <= "00001100"; --display on, cursor off, blink off
 lcd_data <= "00001101"; --display on, cursor off, blink on
 --lcd_data <= "00001110"; --display on, cursor on, blink off
 --lcd_data <= "00001111"; --display on, cursor on, blink on
 --lcd_data <= "00001000"; --display off, cursor off, blink off
 --lcd_data <= "00001001"; --display off, cursor off, blink on
 --lcd_data <= "00001010"; --display off, cursor on, blink off
 --lcd_data <= "00001011"; --display off, cursor on, blink on
 e <= '1';
 states <= initialize;
 ELSIF(clk_count < (120 * freque)) THEN --wait 50 us
 lcd_data <= "00000000";
 e <= '0';
 states <= initialize;
 ELSIF(clk_count < (130 * freque)) THEN --display clear
 lcd_data <= "00000001";
e <= '1';
 states <= initialize;
 ELSIF(clk_count < (2130 * freque)) THEN --wait 2 ms
 lcd_data <= "00000000";
e <= '0';
 states <= initialize;
 ELSIF(clk_count < (2140 * freque)) THEN --entry mode set
 lcd_data <= "00000110"; --increment mode, entire shift off
 --lcd_data <= "00000111"; --increment mode, entire shift on
 --lcd_data <= "00000100"; --decrement mode, entire shift off
 --lcd_data <= "00000101"; --decrement mode, entire shift on
 e <= '1';
 states <= initialize;
 ELSIF(clk_count < (2200 * freque)) THEN --wait 60 us
 lcd_data <= "00000000";
 e <= '0';
 states <= initialize;
 ELSE --initialization complete
 clk_count := 0;
 busy <= '0';
 states <= ready;
 END IF;

 --wait for the enable signal and then latch in the instruction
WHEN ready =>
 IF(lcd_abled = '1' ) THEN
 busy <= '1';
 rs <= lcd_bsy(9);
 --rs<= lcd_rs;
 rw <= lcd_bsy(8);
 --rw <= lcd_rw;
 lcd_data <= lcd_bsy(7 DOWNTO 0);
 --lcd_data <= lcd_bsy;
 clk_count := 0;
 states <= send;
 ELSE
 busy <= '0';
 rs <= '0';
 rw <= '0';
 lcd_data <= "00000000";
 clk_count := 0;
 states <= ready;
 END IF;

 --send instruction to lcd
 WHEN send =>
 busy <= '1';
 IF(clk_count < (50 * freque)) THEN --do not exit for 50us
 busy <= '1';
 IF(clk_count < freque) THEN --negative enable 
 e <= '0';
 ELSIF(clk_count < (14 * freque)) THEN --positive enable half-cycle
 e <= '1';
 ELSIF(clk_count < (27 * freque)) THEN --negative enable half-cycle
 e <= '0';
 END IF;
 clk_count := clk_count + 1;
 states <= send;
 ELSE
 clk_count := 0;
 states <= ready;
 END IF;

 END CASE;

 --reset
IF(reset_all = '0') THEN
 states <= power_up;
END IF;

END IF;
lcd_bsyy<=BUSy;
END PROCESS;
------------------
--Part Two
------------------
PROCESS(clk)
VARIABLE char : INTEGER RANGE 0 TO 10 := 0;
BEGIN
IF(clk'EVENT AND clk = '1' AND game_Over_on ='1') THEN
IF(lcd_bsyy = '0' AND lcd_abled = '0') THEN
lcd_abled <= '1';
IF(char < 12) THEN
char := char + 1;
 END IF;
CASE char IS
WHEN 1 => lcd_bsy <= "1001000111"; --display "G"
WHEN 2 => lcd_bsy <= "1001000001"; --display "A"
WHEN 3 => lcd_bsy <= "1001001101"; --display "M"
WHEN 4 => lcd_bsy <= "1001000101"; --display "E"
WHEN 5 => lcd_bsy <= "1000100000"; --display " "
WHEN 6 => lcd_bsy <= "1001001111"; --display "O"
WHEN 7 => lcd_bsy <= "1001010110"; --display "V"
WHEN 8 => lcd_bsy <= "1001000101"; --display "E"
WHEN 9 => lcd_bsy <= "1001010010"; --display "R"

WHEN OTHERS => lcd_abled <= '0';
END CASE;
ELSE
lcd_abled <= '0';
END IF;
END IF;
END PROCESS;

reset_all <= '1';
lcd_clk <= clk;
	
---------------LCD DISPLAY END	-------------------------------
----------------------------------------------------------------
---------------VGA SYNC ----------------------------------------	
process(clk)  --25MHz clk for vga recondition
begin
       if(clk'event and clk = '1')then
              pixel_clk <= not pixel_clk;
       end if;
		 clk_vga<=pixel_clk;
end process;
	--vga sync start
	n_blank <= '1';  --no direct blanking
	n_sync  <= '0';   --no sync on green
	
	PROCESS(pixel_clk, reset_n)
		VARIABLE h_count	:	INTEGER RANGE 0 TO horizon_period - 1 := 0;  --horizontal counter (counts the columns)
		VARIABLE v_count	:	INTEGER RANGE 0 TO verticl_period - 1 := 0;  --vertical counter (counts the rows)
	BEGIN
	
		IF(reset_n = '0') THEN		--reset asserted
			h_count := 0;				--reset horizontal counter
			v_count := 0;				--reset vertical counter
			h_sync <= NOT h_pol;		--deassert horizontal sync
			v_sync <= NOT v_pol;		--deassert vertical sync
			disp_ena <= '0';			--disable display
			column <= 0;				--reset column pixel coordinate
			row <= 0;					--reset row pixel coordinate
			
		ELSIF(pixel_clk'EVENT AND pixel_clk = '1') THEN

			--counters
			IF(h_count < horizon_period - 1) THEN		--horizontal counter (pixels)
				h_count := h_count + 1;
			ELSE
				h_count := 0;
				IF(v_count < verticl_period - 1) THEN	--veritcal counter (rows)
					v_count := v_count + 1;
				ELSE
					v_count := 0;
				END IF;
			END IF;

			--horizontal sync signal
			IF(h_count < h_pixels + h_fp OR h_count > h_pixels + h_fp + h_pulse) THEN
				h_sync <= NOT h_pol;		--deassert horiztonal sync pulse
			ELSE
				h_sync <= h_pol;			--assert horiztonal sync pulse
			END IF;
			
			--vertical sync signal
			IF(v_count < v_pixels + v_fp OR v_count > v_pixels + v_fp + v_pulse) THEN
				v_sync <= NOT v_pol;		--deassert vertical sync pulse
			ELSE
				v_sync <= v_pol;			--assert vertical sync pulse
			END IF;
			
			--set pixel coordinates
			IF(h_count < h_pixels) THEN  	--horiztonal display time
				column <= h_count;			--set horiztonal pixel coordinate
			END IF;
			IF(v_count < v_pixels) THEN	--vertical display time
				row <= v_count;				--set vertical pixel coordinate
			END IF;

			--set display enable output
			IF(h_count < h_pixels AND v_count < v_pixels) THEN  	--display time
				disp_ena <= '1';											 	--enable display
			ELSE																	--blanking time
				disp_ena <= '0';												--disable display
			END IF;

		END IF;
	END PROCESS;
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
------------------------------VGA SYNC END--------------------------
--------------------------------------------------------------------
------------------------------Original Code-------------------------
------------------------------IMAGE GENERATION----------------------
--horizontal AND vertical  cursor
x <=column;
y <=row;

--reconditioning
process(clk)
begin
     if clk'event and clk='1' then
          recondition_regul<=recondition_next; 
     end if;
end process;
recondition_next<= 0 when recondition_regul= recondition_constant else
recondition_regul+1;
recondition_tick<= '1' when recondition_regul = 0 else
                           '0';
--regular data
process(clk)
begin
     if clk'event and clk='1' then
			
         obstruction1_v_regul<=obstruction1_v_next;
			obstruction2_v_regul<=obstruction2_v_next;
			obstruction3_v_regul<=obstruction3_v_next;
         character_conable_t<=character_conable_t_next;
         obstruction1_l<=obstruction1_l_next;
			obstruction2_l<=obstruction2_l_next;
			obstruction3_l<=obstruction3_l_next;
			over_on<=over_on_next;
      end if;
end process;

--Action Start
process(recondition_tick,obstruction1_l,obstruction2_l,obstruction3_l,obstruction1_v_regul,obstruction2_v_regul,obstruction3_v_regul,character_conable_t,button_j,lose_on)
begin
     character_conable_t_next<=character_conable_t;
	  obstruction1_l_next <=obstruction1_l;
     obstruction1_v_next<=obstruction1_v_regul;
	  obstruction2_l_next <=obstruction2_l;
     obstruction2_v_next<=obstruction2_v_regul;
	  obstruction3_l_next <=obstruction3_l;
     obstruction3_v_next<=obstruction3_v_regul;
	  
     if recondition_tick = '1' then 
			if obstruction1_l >10 then 
				obstruction1_l_next<=obstruction1_l-obstruction1_v;
			else
				obstruction1_l_next<=600;
			end if;
			if obstruction2_l >10 then 
				obstruction2_l_next<=obstruction2_l-obstruction2_v;
			else
				obstruction2_l_next<=625;
			end if;
			if obstruction3_l >10 then 
				obstruction3_l_next<=obstruction3_l-obstruction3_v;
			else
				obstruction3_l_next<=610;
			end if;
	 	
			if button_j='0' and character_conable_t > character_conable_v then 
				character_conable_t_next<=character_conable_t - character_conable_v;
			else
				if character_conable_t > 420 then
					character_conable_t_next<=character_conable_t;
				else
					character_conable_t_next<=character_conable_t + character_conable_v;
				end if;
			end if;
	  end if;
		
		if (obstruction1_l > 100 or  obstruction1_l< 40  or character_conable_t + 20 < obstruction1_t  or character_conable_t > obstruction1_t + obstruction1_h) and
			(obstruction2_l > 100 or  obstruction2_l< 65  or character_conable_t + 20 < obstruction2_t  or character_conable_t > obstruction2_t + obstruction2_h) and
			(obstruction3_l > 100 or  obstruction3_l< 50  or character_conable_t + 20 < obstruction3_t  or character_conable_t > obstruction3_t + obstruction3_h)
		then
			lose_on <= '0';
			over_on_next<=over_on;
		else 
			lose_on <= '1';
			over_on_next <= lose_on;
		end if; 
		
		
		
		if x > game_Over_l and x < (640-game_Over_l) and y> game_Over_t and y < (game_Over_t+ game_Over_k) AND (over_on='1' ) then
			game_Over_on<='1';
		else
			game_Over_on<='0';
		end if;
		
		if x > character_conable_l and x < (character_conable_l+character_conable_w) and y> character_conable_t-character_conable_v and y < (character_conable_t+ character_conable_k)-character_conable_v then
			character_conable_on<='1';
		else
			character_conable_on<='0';
		end if;

		if x > obstruction1_l and x < (obstruction1_l+obstruction1_w) and y> obstruction1_t and y < (obstruction1_t+ obstruction1_h) then
			obstruction1_on<='1';
		else
			obstruction1_on<='0';
		end if;
		
		if x > obstruction2_l and x < (obstruction2_l+obstruction2_w) and y> obstruction2_t and y < (obstruction2_t+ obstruction2_h) then
			obstruction2_on<='1';
		else
			obstruction2_on<='0';
		end if;
		
		if x > obstruction3_l and x < (obstruction3_l+obstruction3_w) and y> obstruction3_t and y < (obstruction3_t+ obstruction3_h) then
			obstruction3_on<='1';
		else
			obstruction3_on<='0';
		end if;
end process;


--game-Over object
red_game_Over  <=(OTHERS => '0');  -- black
blue_game_Over <=(OTHERS => '0');  
green_game_Over<=(OTHERS => '0');  
--characte object
red_character_conable  <=(OTHERS => '1');  
blue_character_conable <=(OTHERS => '1');  
green_character_conable<=(OTHERS => '0');  --red+blue

--obstruction object
red_obstruction1  <=(OTHERS => '1');  
blue_obstruction1 <=(OTHERS => '0');  
green_obstruction1<=(OTHERS => '0');  --red

red_obstruction2  <=(OTHERS => '1');  
blue_obstruction2 <=(OTHERS => '0');  
green_obstruction2<=(OTHERS => '0'); 

red_obstruction3  <=(OTHERS => '1');  
blue_obstruction3 <=(OTHERS => '0');  
green_obstruction3<=(OTHERS => '0'); 

--color setting	
process(clk)
begin
     if clk'event and clk='1' then
         red_regul<=red_next;
			green_regul<=green_next;
			blue_regul<=blue_next;
     end if;
end process;

	--mux
	mux<=disp_ena & game_Over_on & character_conable_on & obstruction1_on  & obstruction2_on & obstruction3_on;    
	with mux select
		red_next <= (OTHERS => '0')            	when "100000",  -- blue + green(background)
		            red_game_Over               	when "110000",
		            red_game_Over               	when "110111",
						red_character_conable         when "101000",
						red_character_conable         when "101001",
						red_character_conable         when "101010",
						red_character_conable         when "101011",
						red_character_conable         when "101100",
						red_character_conable         when "101101",
						red_character_conable         when "101110",
						red_character_conable         when "101111",
		            red_obstruction1              when "100100",
						red_obstruction1              when "100101",
						red_obstruction1              when "100110",
						red_obstruction1              when "100111",
						red_obstruction2              when "100010",
						red_obstruction2              when "100011",
						red_obstruction3              when "100001",
	               (OTHERS => '0')            	when others;
	
	with mux select					
		green_next <= (OTHERS => '1')            	when "100000", 
		            green_game_Over               when "110000",
		            green_game_Over               when "110111",
						green_character_conable       when "101000",
						green_character_conable       when "101001",
						green_character_conable       when "101010",
						green_character_conable       when "101011",
						green_character_conable       when "101100",
						green_character_conable       when "101101",
						green_character_conable       when "101110",
						green_character_conable       when "101111",
		            green_obstruction1            when "100100",
						green_obstruction1            when "100101",
						green_obstruction1            when "100110",
						green_obstruction1            when "100111",
						green_obstruction2            when "100010",
						green_obstruction2            when "100011",
						green_obstruction3            when "100001",
	               (OTHERS => '0')            	when others;
	
	with mux select
		blue_next <= (OTHERS => '1')            	when "100000", 
		            blue_game_Over               	when "110000",
		            blue_game_Over               	when "110111",
						blue_character_conable        when "101000",
						blue_character_conable        when "101001",
						blue_character_conable        when "101010",
						blue_character_conable        when "101011",
						blue_character_conable        when "101100",
						blue_character_conable        when "101101",
						blue_character_conable        when "101110",
						blue_character_conable        when "101111",
		            blue_obstruction1             when "100100",
						blue_obstruction1             when "100101",
						blue_obstruction1             when "100110",
						blue_obstruction1             when "100111",
						blue_obstruction2             when "100010",
						blue_obstruction2             when "100011",
						blue_obstruction3             when "100001",
	               (OTHERS => '0')            	when others;
	--color output
	red  <=red_regul;
	blue <=blue_regul;
	green<=green_regul;
------------------------------IMAGE GENERATION END----------------------
------------------------------Original Code End-------------------------
END behavior;