   with CONFIG; use CONFIG;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   with WORD_PARAMETERS; use WORD_PARAMETERS;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   --with ADDONS_PACKAGE; use ADDONS_PACKAGE;
   with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
   with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
   with WORD_PACKAGE; use WORD_PACKAGE;
   with DICTIONARY_FORM;
   with PUT_EXAMPLE_LINE;
   with LIST_LIST;
   package body LIST_PACKAGE is
   
      package BOOLEAN_IO is new TEXT_IO.ENUMERATION_IO(BOOLEAN);
   
      MM : INTEGER := MAX_MEANING_SIZE;
   
   
      function CAP_STEM(S : STRING) return STRING  is
      begin
         if ALL_CAPS  then
            return UPPER_CASE(S);
         elsif CAPITALIZED  then
            return UPPER_CASE(S(S'FIRST)) & S(S'FIRST+1..S'LAST);
         else
            return S;
         end if;
      end CAP_STEM;
   
      function CAP_ENDING(S : STRING) return STRING  is
      begin
         if ALL_CAPS  then
            return UPPER_CASE(S);
         else
            return S;
         end if;
      end CAP_ENDING;
   
   
      function TRIM_BAR(S : STRING) return STRING is
      begin
         if S'LENGTH >2  and then S(S'FIRST..S'FIRST+2) = "|||"  then
            return TRIM(HEAD(S(S'FIRST+3.. S'LAST), MM));
         elsif S'LENGTH > 1  and then  S(S'FIRST..S'FIRST+1) = "||"  then
            return TRIM(HEAD(S(S'FIRST+2.. S'LAST), MM));
         elsif S(S'FIRST) = '|'  then
            return TRIM(HEAD(S(S'FIRST+1.. S'LAST), MM));
         else
            return TRIM(HEAD(S, MM));
         end if;
      end TRIM_BAR;
   
   
   
--      procedure PUT_CONSTRUCTED_MEANING_LINE(OUTPUT : TEXT_IO.FILE_TYPE;
--                                          DE : in DICTIONARY_ENTRY) is
--         N : INTEGER := 0;
--      begin
--         if PR.IR.QUAL.POFS = NUM  then
--            N := PR.IR.QUAL.NUM.VALUE;
--            TEXT_IO.PUT(OUTPUT, INTEGER'IMAGE(N));
--            case PR.IR.QUAL.NUM.SORT i\s
--               when CARD    =>
--                  TEXT_IO.PUT(OUTPUT, " - (CARD answers 'how many')");
--               when ORD     =>
--                  TEXT_IO.PUT(OUTPUT,
--                              "th - (ORD, 'in series'); (a/the)" & INTEGER'IMAGE(N) &
--                              "th (part) (fract w/pars)");
--               when DIST    =>
--                  TEXT_IO.PUT(OUTPUT,
--                              " each/apiece/times/fold/together/at a time - 'how many each'; by " &
--                              INTEGER'IMAGE(N) & "s ");
--               when ADVERB  =>
--                  TEXT_IO.PUT(OUTPUT,
--                              " times/ways, on" & INTEGER'IMAGE(N) &
--                              " occasions - (ADVERB answers 'how often')");
--               when others  =>
--                  null;
--            end case;
--         end if;
--      
--         TEXT_IO.NEW_LINE(OUTPUT);
--      
--      end PUT_CONSTRUCTED_MEANING_LINE;
--      
   
      function CONSTRUCTED_MEANING(DE : DICTIONARY_ENTRY) return STRING is
         S : STRING(1..MAX_MEANING_SIZE) := NULL_MEANING_TYPE;
         N : INTEGER := 0;
      begin
--TEXT_IO.PUT_LINE("Starting  CONSTRUCTED_MEANING");
         if DE.PART.POFS = NUM  then
            N := DE.KIND.NUM_VALUE;
            
            case DE.PART.NUM.SORT is
               when CARD    =>
                  S := HEAD(INTEGER'IMAGE(N) &  " - (CARD answers 'how many')", MAX_MEANING_SIZE);
               when ORD     =>
                  S := HEAD(INTEGER'IMAGE(N) & "th - (ORD, 'in series'); (a/the)" & INTEGER'IMAGE(N) &
                                               "th (part) (fract w/pars?)", MAX_MEANING_SIZE);
               when DIST    =>
                  S := HEAD(INTEGER'IMAGE(N) & " each/apiece/times/fold/together/at a time - 'how many each'; by " &
                            INTEGER'IMAGE(N) & "s ", MAX_MEANING_SIZE);
               when ADVERB  =>
                  S := HEAD(INTEGER'IMAGE(N) & " times, on" & INTEGER'IMAGE(N) &
                                               " occasions - (ADVERB answers 'how often')", MAX_MEANING_SIZE);
               when others  =>
                  null;
            end case;
         end if;
      
--TEXT_IO.PUT_LINE("Finishing CONSTRUCTED_MEANING");
         return S;
               
      end CONSTRUCTED_MEANING;
   
   
   
      procedure LIST_STEMS(OUTPUT : TEXT_IO.FILE_TYPE;
                           RAW_WORD : STRING;
                           PPA : in out PARSE_ARRAY; PPA_LAST : in out INTEGER) is
         use DICT_IO;
      
         subtype XONS is PART_OF_SPEECH_TYPE range TACKON..SUFFIX;
      
         J, J1, J2, K, KN : INTEGER := 0;
         HIT : BOOLEAN := FALSE;
         DE, NEXT_DE : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY; --  NEXT_DE & ODE ?
         NEXT_PR : PARSE_RECORD := NULL_PARSE_RECORD;
         MEAN  : MEANING_TYPE := NULL_MEANING_TYPE;
         NEXT_MEAN  : MEANING_TYPE := NULL_MEANING_TYPE;
         MEANS : array (1..6) of MEANING_TYPE := (others => NULL_MEANING_TYPE);
         PR : PARSE_RECORD := NULL_PARSE_RECORD;
         OPR : PARSE_RECORD := NULL_PARSE_RECORD;
         ODE : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
         THERE_IS_AN_ADVERB : BOOLEAN := FALSE;
         LINE_NUMBER : INTEGER;
      
         INFLECTION_FREQUENCY : array (FREQUENCY_TYPE) of STRING(1..8) :=
            ("        ",
               "        ",
               "sometime",
               "uncommon",
               "infreq  ",
               "rare    ",
               "veryrare",
               "        ",
               "        ",
               "        " );
         INFLECTION_AGE : array (AGE_TYPE) of STRING(1..7) :=
            ("       ",
               "Archaic",
               "Early  ",
               "       ",
               "Late   ",
               "Later  ",
               "Medievl",
               "Modern ",
               "NeoLat "     );
      
         DICTIONARY_FREQUENCY : array (FREQUENCY_TYPE) of STRING(1..8) :=
            ("        ",
               "        ",
               "        ",
               "        ",     --  "common   ",
               "lesser  ",
               "uncommon",
               "veryrare",
               "inscript",
               "graffiti",
               "Pliny   " );
         DICTIONARY_AGE : array (AGE_TYPE) of STRING(1..7) :=
            ("       ",
               "Archaic",
               "Early  ",
               "       ",
               "Late   ",
               "Later  ",
               "Medievl",
               "Modern ",
               "NeoLat "     );
      
         procedure PAUSE is
            PAUSE_LINE : STRING(1..100);
            PAUSE_LAST : INTEGER := 0;
         begin
            if WORDS_MDEV(PAUSE_IN_SCREEN_OUTPUT)  then
               if METHOD = INTERACTIVE  then
                  if TEXT_IO.NAME(OUTPUT) =
                  TEXT_IO.NAME(TEXT_IO.STANDARD_OUTPUT)  then
                     TEXT_IO.PUT_LINE(TEXT_IO.STANDARD_OUTPUT,
                                      "                          MORE - hit RETURN/ENTER to continue");
                     TEXT_IO.GET_LINE(TEXT_IO.STANDARD_INPUT, PAUSE_LINE, PAUSE_LAST);
                  end if;
               elsif METHOD = COMMAND_LINE_INPUT  then
                  TEXT_IO.PUT_LINE(TEXT_IO.STANDARD_OUTPUT,
                                   "                          MORE - hit RETURN/ENTER to continue");
                  TEXT_IO.GET_LINE(TEXT_IO.STANDARD_INPUT, PAUSE_LINE, PAUSE_LAST);
               elsif METHOD = COMMAND_LINE_FILES  then
                  null;                       --  Do not PAUSE
               end if;
            end if;
         end PAUSE;
      
      
         procedure PUT_INFLECTION_FLAGS is
         begin
            if TRIM(INFLECTION_FREQUENCY(PR.IR.FREQ))'LENGTH /= 0  then
               TEXT_IO.PUT(OUTPUT, " ");
               TEXT_IO.PUT(OUTPUT, INFLECTION_FREQUENCY(PR.IR.FREQ));
            end if;
            if TRIM(INFLECTION_AGE(PR.IR.AGE))'LENGTH /= 0  then
               TEXT_IO.PUT(OUTPUT, " ");
               TEXT_IO.PUT(OUTPUT, INFLECTION_AGE(PR.IR.AGE));
            end if;
         end PUT_INFLECTION_FLAGS;
      
         procedure PUT_DICTIONARY_FLAGS is
         begin
            if PR.IR.QUAL.POFS not in XONS  then
            
               if WORDS_MODE(SHOW_AGE) and then
                  (TRIM(DICTIONARY_AGE(DE.TRAN.AGE))'LENGTH /= 0)  then
                  TEXT_IO.PUT(OUTPUT, " <" & TRIM(DICTIONARY_AGE(DE.TRAN.AGE)) & ">");
               end if;
               if WORDS_MODE(SHOW_FREQUENCY) and then
                  (TRIM(DICTIONARY_FREQUENCY(DE.TRAN.FREQ))'LENGTH /= 0)  then
                  TEXT_IO.PUT(OUTPUT, " <" & TRIM(DICTIONARY_FREQUENCY(DE.TRAN.FREQ)) & ">");
               end if;
            end if;
         end PUT_DICTIONARY_FLAGS;
      
         procedure PUT_DICTIONARY_FORM(OUTPUT : TEXT_IO.FILE_TYPE;
                                       DE : DICTIONARY_ENTRY;
                                       LINE_NUMBER : INTEGER := 0) is
            HIT : BOOLEAN := FALSE;
         begin
            if (PR.IR.QUAL.POFS not in XONS)  and
               (PR.D_K in GENERAL..UNIQUE)           then
            
               if WORDS_MODE(DO_DICTIONARY_FORMS)  then
                  if (PR.D_K /= UNIQUE)                  then
                    if DICTIONARY_FORM(DE)'LENGTH /= 0  then
                      if WORDS_MDEV(DO_PEARSE_CODES) then
                        TEXT_IO.PUT(OUTPUT, "02 ");
                      end if;
                      TEXT_IO.PUT(OUTPUT, DICTIONARY_FORM(DE) & "  ");
                      HIT := TRUE;
                    end if;
                  else
                    if WORDS_MDEV(DO_PEARSE_CODES) then
                      TEXT_IO.PUT(OUTPUT, "02 ");
                    end if;
                    TEXT_IO.PUT(OUTPUT, TRIM(PR.STEM) & "  ");   --  Not sure why I do UNI diff
                    HIT := TRUE;
                  end if; 
               end if;
            
               if WORDS_MDEV(SHOW_DICTIONARY) then
                  TEXT_IO.PUT(OUTPUT, EXT(PR.D_K) & ">");
               end if;
            
            
               if WORDS_MDEV(SHOW_DICTIONARY_LINE)  then
                  if LINE_NUMBER > 0  then
                     TEXT_IO.PUT(OUTPUT, " (" & TRIM(INTEGER'IMAGE(LINE_NUMBER)) & ")");
                     HIT := TRUE;
                  end if;
               end if;
            
            
               if WORDS_MDEV(SHOW_DICTIONARY_CODES) and then
               DE.PART.POFS not in XONS              then
                  TEXT_IO.PUT(OUTPUT, " [");
                  AGE_TYPE_IO.PUT(OUTPUT, DE.TRAN.AGE);
                  AREA_TYPE_IO.PUT(OUTPUT, DE.TRAN.AREA);
                  GEO_TYPE_IO.PUT(OUTPUT, DE.TRAN.GEO);
                  FREQUENCY_TYPE_IO.PUT(OUTPUT, DE.TRAN.FREQ);
                  SOURCE_TYPE_IO.PUT(OUTPUT, DE.TRAN.SOURCE);
                  TEXT_IO.PUT(OUTPUT, "]");
                  HIT := TRUE;
               end if;
            
               PUT_DICTIONARY_FLAGS;        --  HIT ???
            
            
               if HIT   then
                  TEXT_IO.NEW_LINE(OUTPUT);
               end if;
            
            end if;
         
         end PUT_DICTIONARY_FORM;
      
      
      
      begin
      --  Since this procedure weeds out possible parses, if it weeds out all
      --  (or all of a class) it must fix up the rest of the parse array,
      --  e.g., it must clean out dangling prefixes and suffixes
      
--TEXT_IO.PUT_LINE("PPA on entering LIST_STEMS");
--for I in 1..PPA_LAST  loop
--PARSE_RECORD_IO.PUT(PPA(I)); TEXT_IO.NEW_LINE;
--end loop;
      
      
      -------  The gimick of adding an ADV if there is only ADJ VOC  ----
         for I in PPA'FIRST..PPA_LAST  loop
            if PPA(I).IR.QUAL.POFS = ADV   then
               THERE_IS_AN_ADVERB := TRUE;
               exit;
            end if;
         end loop;
      
      
         if not THERE_IS_AN_ADVERB  then
         
            for I in reverse PPA'FIRST..PPA_LAST  loop
            
               if PPA(I).IR.QUAL.POFS = ADJ and then
                  (PPA(I).IR.QUAL.ADJ = ((1, 1), VOC, S, M, POS)    or
                      ((PPA(I).IR.QUAL.ADJ.CS = VOC)   and
                          (PPA(I).IR.QUAL.ADJ.NUMBER = S)   and
                          (PPA(I).IR.QUAL.ADJ.GENDER = M)   and
                          (PPA(I).IR.QUAL.ADJ.CO = SUPER)))    then
               
                  J := I;
               
                  while J >=  PPA'FIRST  loop  --Back through other ADJ cases
                     if PPA(J).IR.QUAL.POFS /= ADJ  then
                        J2 := J;                          --  J2 is first (reverse) that is not ADJ
                        exit;
                     end if;
                     J := J - 1;
                  end loop;
                  while J >=  PPA'FIRST  loop  --  Sweep up associated fixes
                     if PPA(J).IR.QUAL.POFS not in XONS  then
                        J1 := J;                      --  J1 is first (reverse) that is not XONS
                        exit;
                     end if;
                     J := J - 1;
                  end loop;
               
               
               
                  for J in J1+1..J2  loop
                     PPA(PPA_LAST+J-J1+1) := PPA(J);
                  end loop;
                  PPA_LAST := PPA_LAST + J2 - J1 + 1;
                  PPA(PPA_LAST) := PPA(J2+1);
                  PPA(PPA_LAST) := ("e                 ",
                                         ((SUFFIX, NULL_SUFFIX_RECORD), 0, NULL_ENDING_RECORD, X, B),
                                      PPP, NULL_MNPC);
                  PPA_LAST := PPA_LAST + 1;
                  if PPA(J2+1).IR.QUAL.ADJ.CO = POS   then
                     PPA(PPA_LAST) := (PPA(J2+1).STEM,
                                            ((POFS => ADV, ADV => (CO => PPA(J2+1).IR.QUAL.ADJ.CO)),
                                             KEY => 0, ENDING => (1, "e      "), AGE => X, FREQ => B),
                                         PPA(J2+1).D_K,
                                         PPA(J2+1).MNPC);
                     PPP_MEANING :=
                        HEAD("-ly; -ily;  Converting ADJ to ADV",
                             MAX_MEANING_SIZE);
                  
                  elsif PPA(J2+1).IR.QUAL.ADJ.CO = SUPER  then
                     PPA(PPA_LAST) := (PPA(J2+1).STEM,
                                            ((POFS => ADV, ADV => (CO => PPA(J2+1).IR.QUAL.ADJ.CO)),
                                             KEY => 0, ENDING => (2, "me     "), AGE => X, FREQ => B),
                                         PPA(J2+1).D_K,
                                         PPA(J2+1).MNPC);
                     PPP_MEANING :=
                        HEAD("-estly; -estily; most -ly, very -ly  Converting ADJ to ADV",
                             MAX_MEANING_SIZE);
                  end if;
               end if;           --  PPA(I).IR.QUAL.POFS = ADJ
            
            end loop;
         
         end if;           --  not THERE_IS_AN_ADVERB
      
      
         LIST_LIST(PPA(1..PPA_LAST), PPA_LAST);
      
      
      
         if (TEXT_IO.NAME(TEXT_IO.CURRENT_OUTPUT) = TEXT_IO.NAME(TEXT_IO.STANDARD_OUTPUT))  then
            MM := 79;   --  to keep from overflowing screen line
         end if;
      
         --for I in 1..PPA_LAST  loop
         --  PARSE_RECORD_IO.PUT(PPA(I)); TEXT_IO.NEW_LINE;
         --end loop;
      
      
         K := 0;
         ODE := NULL_DICTIONARY_ENTRY;
         OPR := NULL_PARSE_RECORD;
      OUTPUT_LOOP:
         for I in 1..PPA_LAST  loop
            PR := PPA(I);
--TEXT_IO.PUT_LINE("OUTPUT LOOP   0  I = " & INTEGER'IMAGE(I));
         
            if (PR.D_K not in XXX..PPP)  and PR /= NULL_PARSE_RECORD   then
               DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PR.MNPC); 
               DICT_IO.READ(DICT_FILE(PR.D_K), DE);
               LINE_NUMBER := INTEGER(PR.MNPC);
            end if;
--TEXT_IO.PUT_LINE("OUTPUT LOOP   1  I = " & INTEGER'IMAGE(I));
            if I /= PPA_LAST   then
--TEXT_IO.PUT_LINE("OUTPUT LOOP   1a I = " & INTEGER'IMAGE(I));
               NEXT_PR := PPA(I+1);
--TEXT_IO.PUT_LINE("OUTPUT LOOP   1b I = " & INTEGER'IMAGE(I));
               if  (NEXT_PR.D_K in GENERAL..LOCAL)  and NEXT_PR /= NULL_PARSE_RECORD  then
--TEXT_IO.PUT_LINE("OUTPUT LOOP   1A I = " & INTEGER'IMAGE(I));
                  DICT_IO.SET_INDEX(DICT_FILE(PPA(I+1).D_K), PPA(I+1).MNPC);  --  make sure that TRANS is same for multiline?
                  DICT_IO.READ(DICT_FILE(PPA(I+1).D_K), NEXT_DE);
--TEXT_IO.PUT_LINE("OUTPUT LOOP   1AaI = " & INTEGER'IMAGE(I));
               
               else 
--TEXT_IO.PUT_LINE("OUTPUT LOOP   1B I = " & INTEGER'IMAGE(I));
                  NEXT_PR := NULL_PARSE_RECORD;
                  NEXT_DE := NULL_DICTIONARY_ENTRY;
               
--TEXT_IO.PUT_LINE("OUTPUT LOOP   1BaI = " & INTEGER'IMAGE(I));
               end if;
            end if;
--TEXT_IO.PUT_LINE("OUTPUT LOOP   2  I = " & INTEGER'IMAGE(I));
         
         --  Put each remaining parse - stem, ending, then inflection part record
            PUT_INFLECTION:
            begin
--TEXT_IO.PUT_LINE("PUT_INFLECTION   I = " & INTEGER'IMAGE(I));
               if (PR /= OPR  and then (not WORDS_MODE(DO_ONLY_MEANINGS) and
                                        not (CONFIGURATION = MEANINGS)))  and then
               not ((DE.STEMS = ODE.STEMS) and (PR.IR.QUAL = OPR.IR.QUAL))and then  
                 --  Indentical inflections (except for meaning) are sorted together - dont repeat
                  (PR /= NULL_PARSE_RECORD)    then   --  From trimming Archaic/Medieval
                  TEXT_IO.SET_COL(OUTPUT, 1);
                  if WORDS_MDEV(DO_PEARSE_CODES) then
                     if PR.D_K = ADDONS  then
                        TEXT_IO.PUT(OUTPUT, "05 ");
                     elsif PR.D_K in XXX..YYY  then
                        TEXT_IO.PUT(OUTPUT, "06 ");
                     else
                        TEXT_IO.PUT(OUTPUT, "01 ");
                     end if;
                  end if;
               
               
                  TEXT_IO.PUT(OUTPUT, CAP_STEM(TRIM(PR.STEM)));
                  if PR.IR.ENDING.SIZE > 0  then
                     TEXT_IO.PUT(OUTPUT, ".");
                     TEXT_IO.PUT(OUTPUT, CAP_ENDING(PR.IR.ENDING.SUF));
                  end if;
               
               
                  if WORDS_MDEV(DO_PEARSE_CODES) then
                     TEXT_IO.SET_COL(OUTPUT, 23);
                  else
                     TEXT_IO.SET_COL(OUTPUT, 20);
                  end if;
               
               
                  if PR.IR /= NULL_INFLECTION_RECORD  then
                     QUALITY_RECORD_IO.PUT(OUTPUT, PR.IR.QUAL);
                     PUT_INFLECTION_FLAGS;
                     TEXT_IO.NEW_LINE(OUTPUT);
                     PUT_EXAMPLE_LINE(OUTPUT, PR, DE.KIND);    --  Only full when DO_EXAMPLES
                  else
                     TEXT_IO.NEW_LINE(OUTPUT);
                  end if;
               
               elsif (PR = NULL_PARSE_RECORD)    then   --  separating differest pareses (fixes)
                  TEXT_IO.NEW_LINE(OUTPUT);
               end if;
            end PUT_INFLECTION;
         
         
         
            PUT_FORM: begin
--TEXT_IO.PUT_LINE("PUT_FORM         I = " & INTEGER'IMAGE(I));
                --  The Pearse 02 is handled in PUT_DICTIONARY_FORM, why?
               if PR.D_K not in XXX..PPP then
               --  Check to see if the dictionary entry will change and so should be put
               --  or rather that the stems/qual is about to change
                  if (I = PPA_LAST    or else 
                         ((DE.STEMS /= NEXT_DE.STEMS) or (DE.PART /= NEXT_DE.PART)) ) then
                  --  Want situation where forms are same but meaning different
                  --PUT_DICTIONARY_FLAGS;
                     PUT_DICTIONARY_FORM(OUTPUT, DE);
                  
                  end if;
               end if;
            end PUT_FORM;
--TEXT_IO.PUT_LINE("FORM_PUT         I = " & INTEGER'IMAGE(I));
        
--TEXT_IO.PUT_LINE("MEANS ADD  0    I = " & INTEGER'IMAGE(I));
         
            if (DE.PART.POFS = NUM)  and then
                     (DE.PART.NUM.SORT = X)        then
                  --if DE.PART.POFS = NUM  then
              MEAN := CONSTRUCTED_MEANING(DE);
            else
              MEAN := DE.MEAN;
            end if;
--TEXT_IO.PUT_LINE("MEANS ADD  1    I = " & INTEGER'IMAGE(I));
            if (NEXT_DE.PART.POFS = NUM)  and then
               (NEXT_DE.PART.NUM.SORT = X)        then
                   --if NEXT_DE.PART.POFS = NUM  then
              NEXT_MEAN := CONSTRUCTED_MEANING(NEXT_DE);
            else
              NEXT_MEAN := NEXT_DE.MEAN;
            end if;
--TEXT_IO.PUT_LINE("MEANS ADD  2    I = " & INTEGER'IMAGE(I));
            if PR.D_K not in XXX..PPP then
            
               HIT := FALSE;
               for K in 1..KN  loop
                  if MEAN = MEANS(K)  then
                     HIT := TRUE;
                  end if;
               end loop;
--TEXT_IO.PUT_LINE("MEANS ADDING  1 I = " & INTEGER'IMAGE(I));
               if not HIT  then
--TEXT_IO.PUT_LINE("MEANS ADDING  2 I = " & INTEGER'IMAGE(I));
                  KN := KN + 1;
--                  if (PR.IR.QUAL.POFS = NUM)  and then
--                     (PR.IR.QUAL.NUM.KIND = X)  and then
--                     (PR.IR.QUAL.NUM.VALUE > 3)       then
--                    MEANS(KN) := CONSTRUCTED_MEANING(DE);
--                  else
                    MEANS(KN) := MEAN;
--                  end if;
--TEXT_IO.PUT_LINE("MEANS ADDING  3 I = " & INTEGER'IMAGE(I));
                end if;
--TEXT_IO.PUT_LINE("MEANS ADDING  4 I = " & INTEGER'IMAGE(I));
            
            end if;
         
--TEXT_IO.PUT_LINE("MEANS ADDED     I = " & INTEGER'IMAGE(I));
         
            PUT_MEANING: begin
--TEXT_IO.PUT_LINE("PUT_MEANING  0   I = " & INTEGER'IMAGE(I));
               if PR.D_K not in XXX..PPP  then
--TEXT_IO.PUT_LINE("PUT_MEANING  1   I = " & INTEGER'IMAGE(I));
                  if (I = PPA_LAST)  or else 
                     (((DE.STEMS /= NEXT_DE.STEMS) or (DE.PART /= NEXT_DE.PART)) and then 
                         (MEAN /= NEXT_MEAN))then --  Logos situation - diff forms, same meaning - OK
                     --if PR.IR.QUAL.POFS = NUM  and then PR.IR.QUAL.NUM.VALUE > 3 then
--                        if WORDS_MDEV(DO_PEARSE_CODES) then
--                           TEXT_IO.PUT(OUTPUT, "03 ");
--                        end if;
--                        PUT_CONSTRUCTED_MEANING_LINE(OUTPUT, PR);    --  Constructed MEANING
                    -- else
-------------------------------------------------------------------------------------------------------------------
                           for K in 1..KN  loop    --  PUTTING KN MEANS   
                           if WORDS_MDEV(DO_PEARSE_CODES) then
                              TEXT_IO.PUT(OUTPUT, "03 ");
                           end if;
--                           if PR.IR.QUAL.POFS = NUM  and then PR.IR.QUAL.NUM.VALUE > 3 then
--                             PUT_CONSTRUCTED_MEANING_LINE(OUTPUT, PR);    --  Constructed MEANING
--                           else
                             TEXT_IO.PUT_LINE(OUTPUT, TRIM_BAR(MEANS(K)));
--                           end if;
                        end loop;
                        KN := 0;
                        MEANS := (others => NULL_MEANING_TYPE);
                    -- end if;
                  end if;
--TEXT_IO.PUT_LINE("DID_MEANING  1   I = " & INTEGER'IMAGE(I));
               
               else
--TEXT_IO.PUT_LINE("PUT_MEANING  2   I = " & INTEGER'IMAGE(I));
                  if PR.D_K = RRR and then PR.IR.QUAL.POFS = NUM then
                     PUT_DICTIONARY_FLAGS;
                     if WORDS_MDEV(DO_PEARSE_CODES) then
                       TEXT_IO.PUT(OUTPUT, "03 ");
                     end if;
                     TEXT_IO.PUT(OUTPUT, INTEGER'IMAGE(INTEGER(DE.KIND.NUM_VALUE)) &
                                 "  as a ROMAN NUMERAL");      --  Roman Numeral
                     TEXT_IO.NEW_LINE(OUTPUT);
                  elsif PR.D_K = NNN and then NNN_MEANING /= NULL_MEANING_TYPE  then
                     PUT_DICTIONARY_FLAGS;
                     if WORDS_MDEV(DO_PEARSE_CODES) then
                       TEXT_IO.PUT(OUTPUT, "03 ");
                     end if;
                     TEXT_IO.PUT_LINE(OUTPUT, TRIM(NNN_MEANING));  --  Unknown Name
                     NNN_MEANING := NULL_MEANING_TYPE;
                  elsif PR.D_K = XXX and then XXX_MEANING /= NULL_MEANING_TYPE  then
                     PUT_DICTIONARY_FLAGS;
                     if WORDS_MDEV(DO_PEARSE_CODES) then
                       TEXT_IO.PUT(OUTPUT, "06 ");
                     end if;
                     TEXT_IO.PUT_LINE(OUTPUT, TRIM(XXX_MEANING));  --  TRICKS
                     XXX_MEANING := NULL_MEANING_TYPE;
                  elsif PR.D_K = YYY and then YYY_MEANING /= NULL_MEANING_TYPE  then
                     PUT_DICTIONARY_FLAGS;
                     if WORDS_MDEV(DO_PEARSE_CODES) then
                       TEXT_IO.PUT(OUTPUT, "06 ");
                     end if;
                     TEXT_IO.PUT_LINE(OUTPUT, TRIM(YYY_MEANING));  --  Syncope
                     YYY_MEANING := NULL_MEANING_TYPE;
                  elsif PR.D_K = PPP and then PPP_MEANING /= NULL_MEANING_TYPE  then
                     PUT_DICTIONARY_FLAGS;
                     if WORDS_MDEV(DO_PEARSE_CODES) then
                       TEXT_IO.PUT(OUTPUT, "06 ");
                      end if;
                     TEXT_IO.PUT_LINE(OUTPUT, TRIM(PPP_MEANING)); --  Compounds
                     PPP_MEANING := NULL_MEANING_TYPE;
                  end if;
--TEXT_IO.PUT_LINE("DID_MEANING  2   I = " & INTEGER'IMAGE(I));
               
               
               end if;
            end PUT_MEANING;
         
            DO_PAUSE:
            begin
               if I = PPA_LAST  then
                  TEXT_IO.NEW_LINE(OUTPUT);
               elsif (INTEGER(TEXT_IO.LINE(OUTPUT)) >
                      SCROLL_LINE_NUMBER + OUTPUT_SCREEN_SIZE)  then
                  PAUSE;
                  SCROLL_LINE_NUMBER := INTEGER(TEXT_IO.LINE(OUTPUT));
               end if;
            end DO_PAUSE;
         
            ODE := DE;
            OPR := PR;
         end loop OUTPUT_LOOP;
      
      --TEXT_IO.PUT_LINE("End of LIST_STEMS");
      end LIST_STEMS;
   
     procedure LIST_POSSIBLES(OUTPUT : TEXT_IO.FILE_TYPE; 
                              INPUT_WORD : STRING) is
     begin
       null;
     end LIST_POSSIBLES;
      
   end LIST_PACKAGE;
