with TEXT_IO; 
with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with LINE_STUFF; use LINE_STUFF;
procedure MAKEDICT is 
  package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
  use TEXT_IO;
  use DICTIONARY_ENTRY_IO;
  use PART_ENTRY_IO;
  use KIND_ENTRY_IO;
  use TRANSLATION_RECORD_IO;
  use AGE_TYPE_IO;
  use AREA_TYPE_IO;
  use GEO_TYPE_IO;
  use FREQUENCY_TYPE_IO;
  use SOURCE_TYPE_IO;
  use DICT_IO;

  PORTING : constant BOOLEAN := TRUE;
  
  BE_VE : VERB_ENTRY := (CON => (5, 1));
  
  D_K : DICTIONARY_KIND := XXX;       --  ######################


  START_STEM_1  : constant := 1;
  START_STEM_2  : constant := START_STEM_1 + MAX_STEM_SIZE + 1;
  START_STEM_3  : constant := START_STEM_2 + MAX_STEM_SIZE + 1;
  START_STEM_4  : constant := START_STEM_3 + MAX_STEM_SIZE + 1;
  START_PART    : constant := START_STEM_4 + MAX_STEM_SIZE + 1;
  START_TRAN    : constant INTEGER := 
                                      START_PART + 
                                      INTEGER(PART_ENTRY_IO.DEFAULT_WIDTH + 1);
  FINISH_LINE   : constant INTEGER := 
                          START_TRAN +
                          TRANSLATION_RECORD_IO.DEFAULT_WIDTH - 1;
  
      

  

  DICTFILE : DICT_IO.FILE_TYPE;
  INPUT, OUTPUT : TEXT_IO.FILE_TYPE;
  DE : DICTIONARY_ENTRY;

  S, LINE, BLANK_LINE : STRING(1..400) := (others => ' ');
  L, LL, LAST : INTEGER := 0;
  J : DICT_IO.COUNT := 0;
  MEAN_TO_BE : constant MEANING_TYPE := 
    HEAD("to be, exist; also used to form verb perfect passive tenses" &
    " with NOM PERF PPL", MAX_MEANING_SIZE);

begin
  PUT_LINE(
    "Takes a DICTLINE.D_K and produces a STEMLIST.D_K and DICTFILE.D_K");
  PUT("What dictionary to list, GENERAL or SPECIAL  =>");
  GET_LINE(LINE, LAST);
  if LAST > 0  then
    if TRIM(LINE(1..LAST))(1) = 'G'  or else
       TRIM(LINE(1..LAST))(1) = 'g'     then
      D_K := GENERAL;
    elsif TRIM(LINE(1..LAST))(1) = 'S'  or else
          TRIM(LINE(1..LAST))(1) = 's'     then
      D_K := SPECIAL;
    else
      PUT_LINE("No such dictionary");
      raise TEXT_IO.DATA_ERROR;
    end if; 
  end if;


  OPEN(INPUT, IN_FILE, ADD_FILE_NAME_EXTENSION(DICT_LINE_NAME, 
                       DICTIONARY_KIND'IMAGE(D_K))); 
    
if not PORTING  then
               
  CREATE(OUTPUT, OUT_FILE, ADD_FILE_NAME_EXTENSION(STEM_LIST_NAME, 
                           DICTIONARY_KIND'IMAGE(D_K)));
end if;

  CREATE(DICTFILE, OUT_FILE, ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME, 
                             DICTIONARY_KIND'IMAGE(D_K)));

  if D_K = GENERAL  then
    PUT_LINE("MAKEDICT reads DICTLINE.d_k and produces DICTFILE.d_k");
    PUT_LINE("This version inserts ESSE when d_k = GEN");

    J := J + 1;
    
    --  First construct ESSE
    DE.STEMS(1) := "s                 ";
    DE.STEMS(2) := "                  ";
    DE.STEMS(3) := "fu                ";
    DE.STEMS(4) := "fut               ";
    --DE.PART := (PART => V,  CON => (5, 10));
    --DE.PART := (V, ((5, 1)));
    DE.PART := (V, BE_VE);
    DE.KIND := (V, TO_BE);
    DE.TRAN := (X, X, X, A, X);
    DE.MEAN := MEAN_TO_BE;
    

if not PORTING  then
    --  Load ESSE
    for I in 1..4  loop
      PUT(OUTPUT, DE.STEMS(I)); PUT(OUTPUT, ' ');
      PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
      INTEGER_IO.PUT(OUTPUT, I, 2); PUT(OUTPUT, ' ');
--      PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--      PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--      PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--      PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--      PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
      INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
    end loop;
end if;

    WRITE(DICTFILE, DE, J);        --  J = 1
  end if;


    --  Now do the rest 
  OVER_LINES:
  while not END_OF_FILE(INPUT) loop
    S := BLANK_LINE;
    GET_LINE(INPUT, S, LAST);
    if TRIM(S(1..LAST)) /= ""  then
      L := 0;
      
      FORM_DE:
      begin

        DE.STEMS(1) := S(START_STEM_1..MAX_STEM_SIZE);
        --NEW_LINE; PUT(DE.STEMS(1));
        DE.STEMS(2) := S(START_STEM_2..START_STEM_2+MAX_STEM_SIZE-1);
        DE.STEMS(3) := S(START_STEM_3..START_STEM_3+MAX_STEM_SIZE-1);
        DE.STEMS(4) := S(START_STEM_4..START_STEM_4+MAX_STEM_SIZE-1);
        --PUT('#'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
        --PUT('@'); 
        GET(S(START_PART..LAST), DE.PART, L);
        --PUT('%'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
        --PUT('&'); PUT(S(L+1..LAST)); PUT('3'); 
        GET(S(L+1..LAST), DE.PART.POFS, DE.KIND, L);
        GET(S(L+1..LAST), DE.TRAN.AGE, L);
        GET(S(L+1..LAST), DE.TRAN.AREA, L);
        GET(S(L+1..LAST), DE.TRAN.GEO, L);
        GET(S(L+1..LAST), DE.TRAN.FREQ, L);
        GET(S(L+1..LAST), DE.TRAN.SOURCE, L);
        DE.MEAN := HEAD(S(L+2..LAST), MAX_MEANING_SIZE);
      --  Note that this allows initial blanks
      --  L+2 skips over the SPACER, required because this is STRING, not ENUM

      exception
        when others =>
          NEW_LINE;
          PUT_LINE("Exception");
          PUT_LINE(S(1..LAST));
          INTEGER_IO.PUT(INTEGER(J)); NEW_LINE;
          PUT(DE); NEW_LINE;
      end FORM_DE;

      J := J + 1;
      WRITE(DICTFILE, DE, J);


if not PORTING  then

      if DE.PART.POFS = N    and then
         DE.STEMS(1) = DE.STEMS(2)     and then
         DE.STEMS(1) /= ZZZ_STEM       then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 0, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        if DE.STEMS(3) /= NULL_STEM_TYPE  and DE.STEMS(3) /= ZZZ_STEM  then
          PUT(OUTPUT, DE.STEMS(3)); PUT(OUTPUT, ' ');
          PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, 3, 2); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        end if;
        if DE.STEMS(4) /= NULL_STEM_TYPE  and DE.STEMS(4) /= ZZZ_STEM  then
          PUT(OUTPUT, DE.STEMS(4)); PUT(OUTPUT, ' ');
          PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, 4, 2); PUT(OUTPUT, ' ');
          PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
          PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        end if;
      elsif DE.PART.POFS = ADJ  and then
            DE.STEMS(1) = DE.STEMS(2)     and then
            DE.STEMS(1) /= ZZZ_STEM       then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 0, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        if DE.STEMS(3) /= NULL_STEM_TYPE  and DE.STEMS(3) /= ZZZ_STEM  then
          PUT(OUTPUT, DE.STEMS(3)); PUT(OUTPUT, ' ');
          PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, 3, 2); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        end if;
        if DE.STEMS(4) /= NULL_STEM_TYPE  and DE.STEMS(4) /= ZZZ_STEM  then
          PUT(OUTPUT, DE.STEMS(4)); PUT(OUTPUT, ' ');
          PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, 4, 2); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        end if;
      elsif DE.PART.POFS = ADJ  and then
        --  POS taken care of by position
         DE.PART.ADJ.CO = COMP   then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 3, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
      elsif DE.PART.POFS = ADJ  and then
            DE.PART.ADJ.CO = SUPER  then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 4, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
      elsif DE.PART.POFS = ADV  and then
        --  POS taken care of by position
            DE.PART.ADV.CO = COMP   then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 2, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
      elsif DE.PART.POFS = ADV  and then
            DE.PART.ADV.CO = SUPER  then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 3, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
      elsif DE.PART.POFS = V    and then
            DE.STEMS(1) = DE.STEMS(2)     and then
            DE.STEMS(1) /= ZZZ_STEM       then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 0, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        if DE.STEMS(3) /= NULL_STEM_TYPE  and DE.STEMS(3) /= ZZZ_STEM  then
          PUT(OUTPUT, DE.STEMS(3)); PUT(OUTPUT, ' ');
          PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, 3, 2); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        end if;
        if DE.STEMS(4) /= NULL_STEM_TYPE  and DE.STEMS(4) /= ZZZ_STEM  then
          PUT(OUTPUT, DE.STEMS(4)); PUT(OUTPUT, ' ');
          PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, 4, 2); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--          PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
          INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
        end if;
      elsif DE.PART.POFS = NUM  and then
            DE.PART.NUM.SORT = CARD   then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 1, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
      elsif DE.PART.POFS = NUM  and then
            DE.PART.NUM.SORT = ORD    then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 2, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
      elsif DE.PART.POFS = NUM  and then
            DE.PART.NUM.SORT = DIST   then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 3, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
      elsif DE.PART.POFS = NUM  and then
            DE.PART.NUM.SORT = ADVERB  then
        PUT(OUTPUT, DE.STEMS(1)); PUT(OUTPUT, ' ');
        PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, 4, 2); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--        PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
        INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
      else
        for I in 1..4  loop
          if DE.STEMS(I) /= ZZZ_STEM  and 
             DE.STEMS(I) /= NULL_STEM_TYPE  then
            PUT(OUTPUT, DE.STEMS(I)); PUT(OUTPUT, ' ');
            PUT(OUTPUT, DE.PART); PUT(OUTPUT, ' ');
            INTEGER_IO.PUT(OUTPUT, I, 2); PUT(OUTPUT, ' ');
--            PUT(OUTPUT, DE.TRAN.AGE); PUT(OUTPUT, ' ');
--            PUT(OUTPUT, DE.TRAN.AREA); PUT(OUTPUT, ' ');
--            PUT(OUTPUT, DE.TRAN.GEO); PUT(OUTPUT, ' ');
--            PUT(OUTPUT, DE.TRAN.FREQ); PUT(OUTPUT, ' ');
--            PUT(OUTPUT, DE.TRAN.SOURCE); PUT(OUTPUT, ' ');
            INTEGER_IO.PUT(OUTPUT, INTEGER(J), 6); NEW_LINE(OUTPUT);
          end if;
        end loop;
      end if;
end if;   --  PORTING
    end if;
  end loop OVER_LINES;

if not PORTING  then
    CLOSE(OUTPUT);
end if;

exception
  when TEXT_IO.DATA_ERROR  =>
    null;
  when others =>
    PUT_LINE(S(1..LAST));
    INTEGER_IO.PUT(INTEGER(J)); NEW_LINE;
    CLOSE(OUTPUT);

end MAKEDICT;
