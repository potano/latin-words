with TEXT_IO;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with WORD_PARAMETERS; use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE; use ADDONS_PACKAGE;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
with PREFACE;
with WORD_PACKAGE; use WORD_PACKAGE;
with LIST_PACKAGE; use LIST_PACKAGE;
with TRICKS_PACKAGE; use TRICKS_PACKAGE;
with CONFIG; use CONFIG;
pragma Elaborate(WORD_PARAMETERS);
procedure PARSE(COMMAND_LINE : STRING := "") is
  use INFLECTIONS_PACKAGE.INTEGER_IO;
  use INFLECTION_RECORD_IO;
  use TEXT_IO;

  STORAGE_ERROR_COUNT : INTEGER := 0;

  J, K, L : INTEGER := 0;
  LINE, W : STRING(1..2500) := (others => ' ');
  --INPUT : TEXT_IO.FILE_TYPE;

  LINE_NUMBER, WORD_NUMBER : INTEGER := 0;

  PA : PARSE_ARRAY(1..60 ) := (others => NULL_PARSE_RECORD);
  SYNCOPE_MAX : constant := 10;
  SYPA : PARSE_ARRAY(1..SYNCOPE_MAX) := (others => NULL_PARSE_RECORD);
  PA_LAST, SYPA_LAST : INTEGER := 0;


  procedure PARSE_LINE(INPUT_LINE : STRING) is
    L : INTEGER := INPUT_LINE'LAST;
    --LINE : STRING(1..2500) := (others => ' ');
    PPA_LAST : INTEGER := PA_LAST;
  begin
    LINE(1..L) := INPUT_LINE;
    WORD_NUMBER := 0;


  --  Someday I ought to be interested in punctuation and numbers, but not now
  ELIMINATE_NOT_LETTERS:
    begin
    for I in 1..L  loop
      if ((LINE(I) in 'A'..'Z')  or
          (LINE(I) = '-')           or     --  For the comment 
          (LINE(I) = '.')           or     --  Catch period later
          (LINE(I) in 'a'..'z'))  then
        null;
      else
        LINE(I) := ' ';
      end if;
    end loop;
    end ELIMINATE_NOT_LETTERS;



    J := 1;
    K := 0;
    OVER_LINE:
    while J <= L  loop


      if WORDS_MDEV(HAVE_DEBUG_FILE)  and then  WORDS_MDEV(WRITE_DEBUG_FILE)  then
        TEXT_IO.RESET(DBG);
      end if;

      --  Skip over leading and intervening blanks, looking for comments
      --  Punctuation, numbers, and special characters were cleared above
      for I in K+1..L  loop
        exit when LINE(J) in 'A'..'Z';
        exit when LINE(J) in 'a'..'z';
        if I < L  and then
           LINE(I..I+1) = "--"   then
          exit OVER_LINE;      --  the rest of the line is comment
        end if;
        J := I + 1;
      end loop;



----  Skip over leading and intervening blanks
--      --  Punctuation, numbers, and special characters were cleared above
--      for I in K+1..L  loop
--        exit when LINE(J) in 'A'..'Z';
--        exit when LINE(J) in 'a'..'z';
--        J := I + 1;
--      end loop;
      --  If there are trailing blanks, J > L

      exit when J > L;             --  Kludge

      FOLLOWS_PERIOD := FALSE;
      if FOLLOWED_BY_PERIOD  then
        FOLLOWED_BY_PERIOD := FALSE;
        FOLLOWS_PERIOD := TRUE;
      end if;



      CAPITALIZED := FALSE;
      ALL_CAPS := FALSE;


      --  Extract the word
      for I in J..L  loop

--  Although I have removed punctuation above, it may not always be so
        if LINE(I) = '.'  then
          FOLLOWED_BY_PERIOD := TRUE;
          exit;
        end if;
         exit when (LINE(I) = ' ' or LINE(I) = ',' or LINE(I) = '-'
                or LINE(I) = ';' or LINE(I) = ':'
                or LINE(I) = '(' or LINE(I) = '[' or LINE(I) = '{' or LINE(I) = '<'
                or LINE(I) = ')' or LINE(I) = ']' or LINE(I) = '}' or LINE(I) = '>');

        W(I) := LINE(I);
        K := I;

      end loop;



--       if W(J) = UPPER_CASE(W(J))  then
          if W(J) in 'A'..'Z'  and then
             K - J >= 1  and then
             W(J+1) in 'a'..'z'  then
        CAPITALIZED := TRUE;
      end if;

      ALL_CAPS := TRUE;
      for I in J..K  loop
        if W(I) = LOWER_CASE(W(I))  then
          ALL_CAPS := FALSE;
          exit;
        end if;
      end loop;

      for I in J..K-1  loop               --  Kludge for QVAE
        if W(I) = 'Q'  and then W(I+1) = 'V'  then
          W(I+1) := 'U';
        end if;
      end loop;


declare
INPUT_WORD : constant STRING := W(J..K);
ENTERING_PA_LAST : INTEGER := 0;


procedure PASS(INPUT_WORD : STRING) is
begin


      WORD(INPUT_WORD, PA, PA_LAST);


      SYNCOPE(INPUT_WORD, SYPA, SYPA_LAST);  --  Want SYNCOPE second to make cleaner LIST

      PA_LAST := PA_LAST + SYPA_LAST;   --  Make syncope another array to avoid PA-LAST = 0 problems
      PA(1..PA_LAST) := PA(1..PA_LAST-SYPA_LAST) & SYPA(1..SYPA_LAST);  --  Add SYPA to PA
      SYPA(1..SYNCOPE_MAX) := (1..SYNCOPE_MAX => NULL_PARSE_RECORD);   --  Clean up so it does not repeat
      SYPA_LAST := 0;

      ROMAN_NUMERALS(INPUT_WORD, PA, PA_LAST);      --  Roman numerals does not have the problem

      if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT))  and then
        not (WORDS_MDEV(MINIMIZE_OUTPUT) or WORDS_MODE(DO_UNKNOWNS_ONLY))  then
        NEW_LINE;    --?????????????????
      end if;

      if PA_LAST = 0  then    --  WORD failed, try to modify the word
--PUT_LINE("WORDS fail me");
        if WORDS_MODE(DO_TRICKS)  then
--PUT_LINE("DO_TRICKS    ");
          WORDS_MODE(DO_TRICKS) := FALSE;  --  Turn it off so wont be circular
          TRY_TRICKS(INPUT_WORD, PA, PA_LAST, LINE_NUMBER, WORD_NUMBER);
--PUT_LINE("DONE_TRICKS    ");
          --if WORDS_MDEV(DO_MEDIEVAL_TRICKS)  then
            --WORDS_MDEV(DO_MEDIEVAL_TRICKS) := FALSE;  --  Turn it off
            TRY_MEDIEVAL_TRICKS(INPUT_WORD, PA, PA_LAST, LINE_NUMBER, WORD_NUMBER);
            --WORDS_MDEV(DO_MEDIEVAL_TRICKS) := TRUE;   --  Turn it back on
          --end if;
--PUT_LINE("DONE_MEDIEVAL_TRICKS    ");
          WORDS_MODE(DO_TRICKS) := TRUE;   --  Turn it back on
         end if;
      end if;
--PUT_LINE("All TRICKS fail me");

end PASS;

begin

PASS_BLOCK:begin
  PA_LAST := 0;
  WORD_NUMBER := WORD_NUMBER + 1;

PASS(INPUT_WORD);


    ENTERING_PA_LAST := PA_LAST;
    LOOP_OVER_TACKONS:
    for I in 1..3  loop

      REMOVE_A_TACKON:
      declare
        LESS : constant STRING :=
               SUBTRACT_TACKON(INPUT_WORD, TACKONS(I));
      begin
       if LESS  /= INPUT_WORD  then       --  LESS is less
          PASS(LESS);
          if PA_LAST > ENTERING_PA_LAST  then      --  have a possible word
              PA_LAST := PA_LAST + 1;
              PA(ENTERING_PA_LAST+2..PA_LAST) :=
                       PA(ENTERING_PA_LAST+1..PA_LAST-1);
              PA(ENTERING_PA_LAST+1) := (TACKONS(I).TACK,
                      ((TACKON, NULL_TACKON_RECORD), 0, NULL_ENDING_RECORD, X, X),
                        ADDONS,
                        (X, X, X, X, X, TACKONS(I).MNPC));
end if;
          exit LOOP_OVER_TACKONS;
        end if;
      end REMOVE_A_TACKON;
    end loop LOOP_OVER_TACKONS;

end PASS_BLOCK;

      if PA_LAST = 0  then    --  WORD failed, try to modify the word
        if WORDS_MODE(IGNORE_UNKNOWN_NAMES)  and CAPITALIZED  then
--TEXT_IO.PUT_LINE("IGNORE NAMES and CAPITALIZED");
           --  Leading Capital could mean that we have a proper name
--          XXX_MEANING := NULL_MEANING_TYPE;
--          if W(J) in 'A'..'Z'  and then
--             K - J >= 1  and then 
--             W(J+1) in 'a'..'z'  then
             NNN_MEANING := HEAD(
"Assume this is capitalized proper name/abbr, under MODE IGNORE_UNKNOWN_NAME ",
                                  MAX_MEANING_SIZE);
         PA(1) := (HEAD(INPUT_WORD, MAX_STEM_SIZE),
                     ((N, ((0, 0), X, X, X, X)), 0, NULL_ENDING_RECORD, X, X),
                      NNN, NULL_AAMNPC_RECORD);
            PA_LAST := 1;
--TEXT_IO.PUT_LINE("PA_LAST SET TO 1");
        elsif  WORDS_MODE(IGNORE_UNKNOWN_CAPS)  and ALL_CAPS  then
--TEXT_IO.PUT_LINE("IGNORE CAPS and ALL CAPS");
           NNN_MEANING := HEAD(
"Assume this is capitalized proper name/abbr, under MODE IGNORE_UNKNOWN_CAPS ",
                                  MAX_MEANING_SIZE);
            PA(1) := (HEAD(INPUT_WORD, MAX_STEM_SIZE),
                     ((N, ((0, 0), X, X, X, X)), 0, NULL_ENDING_RECORD, X, X),
                      NNN, NULL_AAMNPC_RECORD);
            PA_LAST := 1;
--TEXT_IO.PUT_LINE("PA_LAST SET TO 1");
          --end if;
        end if;
      end if;

--======================================================================

--  At this point we have done what we can with individual words
--  Now see if there is something we can do with word combinations
--  For this we have to look ahead


      if PA_LAST > 0   then    --  But PA may be killed by ALLOW in LIST_STEMS
if WORDS_MODE(DO_COMPOUNDS)  and
   not (CONFIGURATION = MEANINGS)  then
COMPOUNDS_WITH_SUM:
declare
  NW : STRING(1..2500) := (others => ' ');
  NK : INTEGER := 0;

  COMPOUND_TENSE : INFLECTIONS_PACKAGE.TENSE_TYPE := X;
  COMPOUND_TVM   : INFLECTIONS_PACKAGE.TENSE_VOICE_MOOD_RECORD;
  PPL_ON : BOOLEAN := FALSE;

  SUM_INFO : VERB_RECORD := ((5, 1),
                             (X, ACTIVE, X),
                              0,
                              X,
                              TO_BE);

  ESSE_INFO : VERB_RECORD := ((5, 1),
                              (PRES, ACTIVE, INF),
                               0,
                               X,
                               TO_BE);

  PPL_INFO : VPAR_RECORD := ((0, 0),
                              X,
                              X,
                              X,
                             (X, X, X),
                              X);

  SUPINE_INFO : SUPINE_RECORD := ((0, 0),
                                   X,
                                   X,
                                   X,
                                   X);

  procedure LOOK_AHEAD is
    J : INTEGER := 0;
  begin
    for I in K+2..L  loop
    --  Although I have removed punctuation above, it may not always be so
      exit when (LINE(I) = ' ' or LINE(I) = ',' or LINE(I) = '-'
              or LINE(I) = ';' or LINE(I) = ':' or LINE(I) = '.'
              or LINE(I) = '(' or LINE(I) = '[' or LINE(I) = '{' or LINE(I) = '<'
              or LINE(I) = ')' or LINE(I) = ']' or LINE(I) = '}' or LINE(I) = '>');
      J := J + 1;
      NW(J) := LINE(I);
      NK := I;
    end loop;
  end LOOK_AHEAD;

  function NEXT_WORD return STRING is
  begin
    return TRIM(NW);
  end NEXT_WORD;

  function IS_SUM(T : STRING) return BOOLEAN is
    SA : constant array (MOOD_TYPE range IND..SUB,
                         TENSE_TYPE range PRES..FUTP,
                         NUMBER_TYPE range S..P,
                         PERSON_TYPE range 1..3)
                                                    of STRING(1..9) :=
(
 (         --  IND
(("sum      ", "es       ", "est      "), ("sumus    ", "estis    ", "sunt     ")),
(("eram     ", "eras     ", "erat     "), ("eramus   ", "eratis   ", "erant    ")),
(("ero      ", "eris     ", "erit     "), ("erimus   ", "eritis   ", "erunt    ")),
(("fui      ", "fuisti   ", "fuit     "), ("fuimus   ", "fuistis  ", "fuerunt  ")),
(("fueram   ", "fueras   ", "fuerat   "), ("fueramus ", "fueratis ", "fuerant  ")),
(("fuero    ", "fueris   ", "fuerit   "), ("fuerimus ", "fueritis ", "fuerunt  "))
 ),
 (         --  SUB
(("sim      ", "sis      ", "sit      "), ("simus    ", "sitis    ", "sint     ")),
(("essem    ", "esses    ", "esset    "), ("essemus  ", "essetis  ", "essent   ")),
(("zzz      ", "zzz      ", "zzz      "), ("zzz      ", "zzz      ", "zzz      ")),
(("fuerim   ", "fueris   ", "fuerit   "), ("fuerimus ", "fueritis ", "fuerint  ")),
(("fuissem  ", "fuisses  ", "fuisset  "), ("fuissemus", "fuissetis", "fuissent ")),
(("zzz      ", "zzz      ", "zzz      "), ("zzz      ", "zzz      ", "zzz      "))
 )
);

  begin
    if T = ""  then
      return FALSE;
    elsif T(T'FIRST) /= 's'  and
          T(T'FIRST) /= 'e'  and
          T(T'FIRST) /= 'f'      then
    return FALSE;
    end if;
    for L in MOOD_TYPE range IND..SUB  loop
      for K in TENSE_TYPE range PRES..FUTP  loop
        for J in NUMBER_TYPE range S..P  loop
          for I in PERSON_TYPE range 1..3  loop
            if TRIM(T) = TRIM(SA(L, K, J, I))  then
              SUM_INFO := ((5, 1), (K, ACTIVE, L), I, J, TO_BE);
              return TRUE;     --  Only one of the forms can agree
            end if;
          end loop;
        end loop;
      end loop;
    end loop;
    return FALSE;
  end IS_SUM;

  function IS_ESSE(T : STRING) return BOOLEAN is
  begin
    return TRIM(T) = "esse";
  end IS_ESSE;

  function IS_FUISSE(T : STRING) return BOOLEAN is
  begin
    return TRIM(T) = "fuisse";
  end IS_FUISSE;

  function IS_IRI(T : STRING) return BOOLEAN is
  begin
    return TRIM(T) = "iri";
  end IS_IRI;


begin

  --  Look ahead for sum                                           
LOOK_AHEAD;
if IS_SUM(NEXT_WORD)  then                 --  On NEXT_WORD = sum, esse, iri

    for I in 1..PA_LAST  loop    --  Check for PPL
      if PA(I).IR.QUAL.PART = VPAR and then
         PA(I).IR.QUAL.VPAR.CS = NOM  and then
         PA(I).IR.QUAL.VPAR.NUMBER = SUM_INFO.NUMBER  and then
      ( (PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (PERF, PASSIVE, PPL)) or
        (PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT,  ACTIVE,  PPL)) or
        (PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT,  PASSIVE, PPL)) )  then

        --  There is at least one hit, fix PA, and advance J over the sum
        K := NK;

      end if;
    end loop;

    if K = NK  then      --  There was a PPL hit
      CLEAR_PAS_NOM_PPL:
      declare
        J : INTEGER := PA_LAST;
      begin
        while J >= 1  loop        --  Sweep backwards to kill empty suffixes
          if ((PA(J).IR.QUAL.PART = PREFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.PART = SUFFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.PART = TACKON) and then (PPL_ON))  then
            null;



          elsif PA(J).IR.QUAL.PART = VPAR and then
             PA(J).IR.QUAL.VPAR.CS = NOM  and then
             PA(J).IR.QUAL.VPAR.NUMBER = SUM_INFO.NUMBER  then

            if PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (PERF, PASSIVE, PPL)  then
            PPL_ON := TRUE;

             case SUM_INFO.TENSE_VOICE_MOOD.TENSE is  --  Allows PERF for sum
                when PRES | PERF  =>  COMPOUND_TENSE := PERF;
                when IMPF | PLUP  =>  COMPOUND_TENSE := PLUP;
                when FUT          =>  COMPOUND_TENSE := FUTP;
                when others       =>  COMPOUND_TENSE := X;
              end case;
           COMPOUND_TVM := (COMPOUND_TENSE, PASSIVE, SUM_INFO.TENSE_VOICE_MOOD.MOOD);

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD,
                        PA(J).IR.QUAL.VPAR.KIND);
      PPP_MEANING :=
          HEAD("PERF PASSIVE PPL + verb TO_BE => PASSIVE perfect system",
                MAX_MEANING_SIZE);

            elsif PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT, ACTIVE,  PPL)  then
            PPL_ON := TRUE;
            COMPOUND_TENSE := SUM_INFO.TENSE_VOICE_MOOD.TENSE;
           COMPOUND_TVM := (COMPOUND_TENSE, ACTIVE, SUM_INFO.TENSE_VOICE_MOOD.MOOD);

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD,
                        PA(J).IR.QUAL.VPAR.KIND);
      PPP_MEANING := HEAD(
     "FUT ACTIVE PPL + verb TO_BE => ACTIVE Periphrastic - about to, going to",
                MAX_MEANING_SIZE);

            elsif PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT, PASSIVE, PPL)  then
            PPL_ON := TRUE;
            COMPOUND_TENSE := SUM_INFO.TENSE_VOICE_MOOD.TENSE;
           COMPOUND_TVM := (COMPOUND_TENSE, PASSIVE, SUM_INFO.TENSE_VOICE_MOOD.MOOD);

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD,
                        PA(J).IR.QUAL.VPAR.KIND);
      PPP_MEANING := HEAD(
  "FUT PASSIVE PPL + verb TO_BE => PASSIVE Periphrastic - should/ought/had to",
                MAX_MEANING_SIZE);

            end if;
          else
            PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
            PA_LAST := PA_LAST - 1;
            PPL_ON := FALSE;
          end if;
          J := J - 1;
        end loop;
      end CLEAR_PAS_NOM_PPL;


      PA_LAST := PA_LAST + 1;
      PA(PA_LAST) :=
          (HEAD("PPL+" & NEXT_WORD, MAX_STEM_SIZE),
                ((V,
                    (PPL_INFO.CON,
                     COMPOUND_TVM,
                     SUM_INFO.PERSON,
                     SUM_INFO.NUMBER,
                     PPL_INFO.KIND)
                 ), 0, NULL_ENDING_RECORD, X, A),
                    PPP, NULL_AAMNPC_RECORD);

    end if;

elsif IS_ESSE(NEXT_WORD) or IS_FUISSE(NEXT_WORD)  then     --  On NEXT_WORD

    for I in 1..PA_LAST  loop    --  Check for PPL
      if PA(I).IR.QUAL.PART = VPAR and then
      (((PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (PERF, PASSIVE, PPL)) and
                                                   IS_ESSE(NEXT_WORD)) or
       ((PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT,  ACTIVE,  PPL)) or
        (PA(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT,  PASSIVE, PPL))) )  then

        --  There is at least one hit, fix PA, and advance J over the sum
        K := NK;

      end if;
    end loop;

    if K = NK  then      --  There was a PPL hit
      CLEAR_PAS_PPL:
      declare
        J : INTEGER := PA_LAST;
      begin
        while J >= 1  loop        --  Sweep backwards to kill empty suffixes
          if ((PA(J).IR.QUAL.PART = PREFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.PART = SUFFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.PART = TACKON) and then (PPL_ON))  then
            null;



          elsif PA(J).IR.QUAL.PART = VPAR   then

            if PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (PERF, PASSIVE, PPL)  then
            PPL_ON := TRUE;

           COMPOUND_TVM := (PERF, PASSIVE, INF);

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD,
                        PA(J).IR.QUAL.VPAR.KIND);
            PPP_MEANING :=
                HEAD("PERF PASSIVE PPL + esse => PERF PASSIVE INF",
                      MAX_MEANING_SIZE);

            elsif PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT, ACTIVE,  PPL)  then
            PPL_ON := TRUE;
           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD,
                        PA(J).IR.QUAL.VPAR.KIND);
            if IS_ESSE(NEXT_WORD)  then
              COMPOUND_TVM := (PRES, ACTIVE, INF);
      PPP_MEANING := HEAD(
     "FUT ACTIVE PPL + esse => PRES Periphastic/FUT ACTIVE INF - be about/going to",
                MAX_MEANING_SIZE);
              -- also peri COMPOUND_TVM := (PRES, ACTIVE, INF);
            else   --  fuisse
              COMPOUND_TVM := (PERF, ACTIVE, INF);
     PPP_MEANING := HEAD(
     "FUT ACT PPL+fuisse => PERF ACT INF Periphrastic - to have been about/going to",
                MAX_MEANING_SIZE);
            end if;


            elsif PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD = (FUT, PASSIVE, PPL)  then
            PPL_ON := TRUE;

           PPL_INFO := (PA(J).IR.QUAL.VPAR.CON,   --  In this case, there is 1 
                        PA(J).IR.QUAL.VPAR.CS,    --  although several different
                        PA(J).IR.QUAL.VPAR.NUMBER,--  dictionary entries may fit
                        PA(J).IR.QUAL.VPAR.GENDER,--  all have same PPL_INFO
                        PA(J).IR.QUAL.VPAR.TENSE_VOICE_MOOD,
                        PA(J).IR.QUAL.VPAR.KIND);
            if IS_ESSE(NEXT_WORD)  then
              COMPOUND_TVM := (PRES, PASSIVE, INF);
      PPP_MEANING := HEAD(
     "FUT PASSIVE PPL + esse => PRES PASSIVE INF",
                MAX_MEANING_SIZE);
              -- also peri COMPOUND_TVM := (PRES, ACTIVE, INF);
            else   --  fuisse
              COMPOUND_TVM := (PERF, PASSIVE, INF);
     PPP_MEANING := HEAD(
     "FUT PASSIVE PPL + fuisse => PERF PASSIVE INF Periphrastic - about to, going to",
                MAX_MEANING_SIZE);
            end if;


            end if;
          else
            PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
            PA_LAST := PA_LAST - 1;
            PPL_ON := FALSE;
          end if;
          J := J - 1;
        end loop;
      end CLEAR_PAS_PPL;


      PA_LAST := PA_LAST + 1;
      PA(PA_LAST) :=
          (HEAD("PPL+" & NEXT_WORD, MAX_STEM_SIZE),
                ((V,
                    (PPL_INFO.CON,
                     COMPOUND_TVM,
                     0,
                     X,
                     PPL_INFO.KIND)
                 ), 0, NULL_ENDING_RECORD, X, A),
                    PPP, NULL_AAMNPC_RECORD);

    end if;

elsif IS_IRI(NEXT_WORD)  then              --  On NEXT_WORD = sum, esse, iri
  --  Look ahead for sum                                           

    for J in 1..PA_LAST  loop    --  Check for SUPINE
      if PA(J).IR.QUAL.PART = SUPINE   and then
         PA(J).IR.QUAL.SUPINE.CS = ACC    then
         --  There is at least one hit, fix PA, and advance J over the iri
        K := NK;

      end if;
    end loop;

    if K = NK  then      --  There was a SUPINE hit
      CLEAR_PAS_SUPINE:
      declare
        J : INTEGER := PA_LAST;
      begin
        while J >= 1  loop        --  Sweep backwards to kill empty suffixes
          if ((PA(J).IR.QUAL.PART = PREFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.PART = SUFFIX) and then (PPL_ON))  then
            null;
          elsif ((PA(J).IR.QUAL.PART = TACKON) and then (PPL_ON))  then
            null;



          elsif PA(J).IR.QUAL.PART = SUPINE  and then
                PA(J).IR.QUAL.SUPINE.CS = ACC  then

            PPL_ON := TRUE;
         SUPINE_INFO := (PA(J).IR.QUAL.SUPINE.CON,
                         PA(J).IR.QUAL.SUPINE.CS,
                         PA(J).IR.QUAL.SUPINE.NUMBER,
                         PA(J).IR.QUAL.SUPINE.GENDER,
                         PA(J).IR.QUAL.SUPINE.KIND);



      PA_LAST := PA_LAST + 1;
      PA(PA_LAST) :=
          (HEAD("SUPINE + iri", MAX_STEM_SIZE),
                ((V,
                    (SUPINE_INFO.CON,
                     (FUT, PASSIVE, INF),
                     0,
                     X,
                     SUPINE_INFO.KIND)
                 ), 0, NULL_ENDING_RECORD, X, A),
                    PPP, NULL_AAMNPC_RECORD);
      PPP_MEANING := HEAD(
     "SUPINE + iri => FUT PASSIVE INF - to be about/going/ready to be ~",
                MAX_MEANING_SIZE);

            K := NK;


          else
            PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
            PA_LAST := PA_LAST - 1;
            PPL_ON := FALSE;
          end if;
          J := J -1;
        end loop;
      end CLEAR_PAS_SUPINE;
    end if;

end if;       --  On NEXT_WORD = sum, esse, iri


end COMPOUNDS_WITH_SUM;
end if;       --  On WORDS_MODE(DO_COMPOUNDS)


--========================================================================

--TEXT_IO.PUT_LINE("Before LISTing STEMS (PA_LAST > 0 to start) PA_LAST = " & 
--INTEGER'IMAGE(PA_LAST));

        if  not WORDS_MODE(DO_UNKNOWNS_ONLY)      then
          PPA_LAST := PA_LAST;
          if  WORDS_MODE(WRITE_OUTPUT_TO_FILE)      then
            LIST_STEMS(OUTPUT, INPUT_WORD, PA, PPA_LAST);
          else
            LIST_STEMS(CURRENT_OUTPUT, INPUT_WORD, PA, PPA_LAST);
          end if;
          PA_LAST := PPA_LAST;  --  May have killed PA in LIST
        end if;
      end if;

--TEXT_IO.PUT_LINE("After LISTing STEMS (PA_LAST > 0 to start) PA_LAST = " & 
--INTEGER'IMAGE(PA_LAST));

      if PA_LAST = 0   then

        if  WORDS_MODE(WRITE_OUTPUT_TO_FILE)      then
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(OUTPUT, "04 ");
          end if;
          TEXT_IO.PUT(OUTPUT, INPUT_LINE(J..K)); TEXT_IO.SET_COL(OUTPUT, 30);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(OUTPUT, LINE_NUMBER, 5);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(OUTPUT, WORD_NUMBER, 3);
          TEXT_IO.PUT_LINE(OUTPUT, "    ========   UNKNOWN    ");
          TEXT_IO.NEW_LINE(OUTPUT);
        else              --  Just screen output
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT("04 ");
          end if;
          TEXT_IO.PUT(INPUT_LINE(J..K));
          TEXT_IO.SET_COL(30);
          TEXT_IO.PUT_LINE("    ========   UNKNOWN    ");
          TEXT_IO.NEW_LINE;
        end if;

        if WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
          if WORDS_MDEV(INCLUDE_UNKNOWN_CONTEXT) or
             WORDS_MDEV(DO_ONLY_INITIAL_WORD)  then
            TEXT_IO.PUT_LINE(UNKNOWNS, INPUT_LINE);
          end if;
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(UNKNOWNS, "04 ");
          end if;
          TEXT_IO.PUT(UNKNOWNS, INPUT_LINE(J..K));
          TEXT_IO.SET_COL(UNKNOWNS, 30);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(UNKNOWNS, LINE_NUMBER, 5);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(UNKNOWNS, WORD_NUMBER, 3);
          TEXT_IO.PUT_LINE(UNKNOWNS, "    ========   UNKNOWN    ");
        end if;
      end if;

      if PA_LAST = 0   then
        if WORDS_MODE(DO_STEMS_FOR_UNKNOWN)  and
          (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT))  then  --  Maybe not necessary
          LIST_POSSIBLES(CURRENT_OUTPUT, INPUT_WORD);         --  Need to do the OUTPUT thing
        end if;
      end if;

      if PA_LAST = 0   then
        if WORDS_MDEV(UPDATE_LOCAL_DICTIONARY)  and  -- Don't if reading from file
          (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT))  then
          UPDATE_LOCAL_DICTIONARY_FILE;
          WORD(INPUT_WORD, PA, PA_LAST);       --  Circular if you dont update!!!!!
        end if;
      end if;

      PA_LAST := 0;

end;
----------------------------------------------------------------------
----------------------------------------------------------------------


      J := K + 1;    --  In case it is end of line and we don't look for ' '

      exit when WORDS_MDEV(DO_ONLY_INITIAL_WORD);

    end loop OVER_LINE;        --  Loop on line

exception
  --   Have STORAGE_ERROR check in WORD too  ?????????????
  when STORAGE_ERROR  =>    --  I want to again, at least twice
    if WORDS_MDEV(DO_PEARSE_CODES) then
      TEXT_IO.PUT("00 ");
    end if;
    TEXT_IO.PUT_LINE(    --  ERROR_FILE,
                          "STORAGE_ERROR Exception in WORDS, try again");
    STORAGE_ERROR_COUNT := STORAGE_ERROR_COUNT + 1;
    if STORAGE_ERROR_COUNT >= 4  then  raise; end if;
    PA_LAST := 0;
  when GIVE_UP =>
    PA_LAST := 0;
    raise;
  when others  =>    --  I want to try to get on with the next line
    TEXT_IO.PUT_LINE(    --  ERROR_FILE,
                          "Exception in PARSE_LINE processing " & INPUT_LINE);
        if WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(UNKNOWNS, "00 ");
          end if;
          TEXT_IO.PUT(UNKNOWNS, INPUT_LINE(J..K));
          TEXT_IO.SET_COL(UNKNOWNS, 30);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(UNKNOWNS, LINE_NUMBER, 5);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(UNKNOWNS, WORD_NUMBER, 3);
          TEXT_IO.PUT_LINE(UNKNOWNS, "    ========   ERROR      ");
        end if;
      PA_LAST := 0;
end PARSE_LINE;

begin              --  PARSE
--  All Rights Reserved   -   William Armstrong Whitaker

  INITIALIZE_WORD_PARAMETERS;
  INITIALIZE_DEVELOPER_PARAMETERS;
  INITIALIZE_WORD_PACKAGE;

  if METHOD = COMMAND_LINE_INPUT  then
    if TRIM(COMMAND_LINE) /= ""  then
      PARSE_LINE(COMMAND_LINE);
    end if;

  else

  PREFACE.PUT_LINE(
"Copyright (c) 1993-1999 - Free for your use - Version 1.95");
  PREFACE.PUT_LINE(
"Updates every few months at http://www.erols.com/whitaker/words.htm");
  PREFACE.PUT_LINE(
"Comments? William Whitaker, Box 3036, McLean VA 22103 USA - whitaker@erols.com");
  PREFACE.NEW_LINE;
  PREFACE.PUT_LINE(
"Input a word or line of Latin to get the forms and meanings");
  PREFACE.PUT_LINE("    Or input " & START_FILE_CHARACTER &
           " and the name of a file containing words or lines");
  PREFACE.PUT_LINE("    Or input " & CHANGE_PARAMETERS_CHARACTER &
           " to change parameters and mode of the program");
  PREFACE.PUT_LINE("    Or input " & HELP_CHARACTER &
           " to get help wherever available on individual parameters");
  PREFACE.PUT_LINE(
"An empty line (just a RETURN or ENTER) from the keyboard exits the program");

  if CONFIGURATION = MEANINGS  then
    PREFACE.PUT_LINE(
            "THIS VERSION IS HARDWIRED TO GIVE MEANINGS ONLY, NO INFLECTIONS");
  end if;

  loop
    begin                    --  Block to manipulate file of lines
      if (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT))  then
        SCROLL_LINE_NUMBER := INTEGER(TEXT_IO.LINE(TEXT_IO.STANDARD_OUTPUT));
        PREFACE.NEW_LINE;
        PREFACE.PUT("=>");
      end if;

      GET_LINE(LINE, L);
      if (L = 0) then
        if (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT))  then
          exit;
        else
          LINE_NUMBER := LINE_NUMBER + 1;  --  Count blank lines of file
          if END_OF_FILE(CURRENT_INPUT) then
            SET_INPUT(STANDARD_INPUT);
            CLOSE(INPUT);
          end if;
        end if;

      else

        if LINE(1) = START_FILE_CHARACTER  then    --  To begin file of words
          if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT)) then
            TEXT_IO.PUT_LINE("Cannot have file of words (@FILE) in an @FILE");
          else
            TEXT_IO.OPEN(INPUT, TEXT_IO.IN_FILE, TRIM(LINE(2..L)));
            TEXT_IO.SET_INPUT(INPUT);
          end if;
        elsif LINE(1) = CHANGE_PARAMETERS_CHARACTER  and then
              (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT)) and then
              not CONFIG.SUPPRESS_PREFACE  then
          CHANGE_PARAMETERS;
        elsif --  CONFIGURATION = DEVELOPER_VERSION  and then    --  Allow anyone to do it
              LINE(1) = CHANGE_DEVELOPER_MODES_CHARACTER  and then
              (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT)) and then
              not CONFIG.SUPPRESS_PREFACE  then
          CHANGE_DEVELOPER_MODES;
        else
          if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT))  then
            PREFACE.NEW_LINE;
            PREFACE.PUT_LINE(LINE(1..L));
          end if;
          if WORDS_MODE(WRITE_OUTPUT_TO_FILE)     then
            if not CONFIG.SUPPRESS_PREFACE     then
              NEW_LINE(OUTPUT);
              TEXT_IO.PUT_LINE(OUTPUT, LINE(1..L));
            end if;
          end if;
          LINE_NUMBER := LINE_NUMBER + 1;  --  Count only lines to be parsed
          PARSE_LINE(LINE(1..L));
        end if;

      end if;
    exception
      when NAME_ERROR | USE_ERROR =>
        if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT))  then
          SET_INPUT(STANDARD_INPUT);
          CLOSE(INPUT);
        end if;
        PUT_LINE("An unknown or unacceptable file name. Try Again");
      when END_ERROR =>          --  The end of the input file resets to CON:
        if (NAME(CURRENT_INPUT) /= NAME(STANDARD_INPUT))  then
          SET_INPUT(STANDARD_INPUT);
          CLOSE(INPUT);
          if METHOD = COMMAND_LINE_FILES  then raise GIVE_UP; end if;
        else
          PUT_LINE("Raised END_ERROR, although in STANDARD_INPUT");
          PUT_LINE("^Z is inappropriate keyboard input, WORDS should be terminated with a blank line");
          raise GIVE_UP;
        end if;
      when STATUS_ERROR =>      --  The end of the input file resets to CON:
          PUT_LINE("Raised STATUS_ERROR");
    end;                     --  end Block to manipulate file of lines

  end loop;          --  Loop on lines

  end if;     --  On command line input

  begin
    STEM_IO.OPEN(STEM_FILE(LOCAL), STEM_IO.IN_FILE,
                              ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                                                      "LOCAL"));
          --  Failure to OPEN will raise an exception, to be handled below
    if STEM_IO.IS_OPEN(STEM_FILE(LOCAL)) then
      STEM_IO.DELETE(STEM_FILE(LOCAL));
    end if;
  exception
    when others =>
      null;      --  If cannot OPEN then it does not exist, so is deleted
  end;
  --  The rest of this seems like overkill, it might have been done elsewhere
  begin
    if
      DICT_IO.IS_OPEN(DICT_FILE(LOCAL)) then
      DICT_IO.DELETE(DICT_FILE(LOCAL));
    else
      DICT_IO.OPEN(DICT_FILE(LOCAL), DICT_IO.IN_FILE,
                              ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME,
                                                      "LOCAL"));
      DICT_IO.DELETE(DICT_FILE(LOCAL));
    end if;
  exception when others => null; end;   --  not there, so don't have to DELETE
  begin
    if
      DICT_IO.IS_OPEN(DICT_FILE(ADDONS))  then
      DICT_IO.DELETE(DICT_FILE(ADDONS));
    else
      DICT_IO.OPEN(DICT_FILE(ADDONS), DICT_IO.IN_FILE,
                              ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME,
                                                      "ADDONS"));
     DICT_IO.DELETE(DICT_FILE(ADDONS));
    end if;
  exception when others => null; end;   --  not there, so don't have to DELETE
  begin
    if
      DICT_IO.IS_OPEN(DICT_FILE(UNIQUE)) then
      DICT_IO.DELETE(DICT_FILE(UNIQUE));
    else
      DICT_IO.OPEN(DICT_FILE(UNIQUE), DICT_IO.IN_FILE,
                              ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME,
                                                      "UNIQUE"));
      DICT_IO.DELETE(DICT_FILE(UNIQUE));
    end if;
  exception when others => null; end;   --  not there, so don't have to DELETE

exception
  when STORAGE_ERROR  =>    --  Have tried at least twice, fail
    PREFACE.PUT_LINE("Continuing STORAGE_ERROR Exception in PARSE");
    PREFACE.PUT_LINE("If insufficient memory in DOS, try removing TSRs");
  when GIVE_UP  =>
    PREFACE.PUT_LINE("Giving up!");
  when others  =>
    PREFACE.PUT_LINE("Unexpected exception raised in PARSE");
end PARSE;