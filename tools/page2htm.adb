with TEXT_IO; use TEXT_IO;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
procedure PAGE2HTM is
  LINE, LINE_OUT, BLANK_LINE : STRING(1..300) := (others => ' ');
  LAST, COLONS : INTEGER := 0;
  CONT : BOOLEAN := FALSE;
  
  INPUT, OUTPUT : FILE_TYPE;
  
begin 
  PUT_LINE("DICTPAGE.RAW (sorted) -> DICTPAGE.HTM");
  PUT_LINE("For use in preparing a DICTPAGE.HTM after running DICTPAGE and sorting.");
  
  OPEN(INPUT, IN_FILE, "DICTPAGE.RAW");
  CREATE(OUTPUT, OUT_FILE, "DICTPAGE.HTM");
  
  while not END_OF_FILE(INPUT)  loop
    if not CONT  then          --  Skips line read from | continuation
      GET_LINE(INPUT, LINE, LAST);
    else
      CONT := FALSE;
    end if;
    if LINE(1) /= '#'  then
        PUT_LINE("BAD LINE   >" & LINE(1..LAST));
    end if;
    for I in 1..LAST  loop
      if LINE(I) = '['  then
        null;
      end if;
      if LINE(I+8..I+9) = "::"  then
        if LINE(I+11) /= '|'  then  
          PUT(OUTPUT, "<B>" & TRIM(LINE(2..I-1)) & "</B>  ");
          PUT_LINE(OUTPUT, TRIM(LINE(I..I+6) & "<BR>"));
          PUT_LINE(OUTPUT, TRIM(LINE(I+11..LAST)) & "<BR>");
          exit;
        else                           --  To do | continuations
          for J in 1..9  loop
            if LINE(I+10+J) = '|'  then
              PUT_LINE(OUTPUT, TRIM(LINE(I+11+J..LAST)) & "<BR>");
              CONT := TRUE;
              GET_LINE(INPUT, LINE, LAST);
            else
--             CONT := FALSE;
             exit;
            end if;
--            if CONT then  GET_LINE(INPUT, LINE, LAST);  end if;
          end loop;
        end if;
      end if;
    end loop;  --  On LINE
    
  end loop;  --  On file
  
end PAGE2HTM;

 
