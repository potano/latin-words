with CONFIG; use CONFIG;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with WORD_PARAMETERS; use WORD_PARAMETERS;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE; use ADDONS_PACKAGE;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with WORD_PACKAGE; use WORD_PACKAGE;
with DICTIONARY_FORM;
with PUT_EXAMPLE_LINE;
with LIST_LIST;
package body LIST_PACKAGE is

  package BOOLEAN_IO is new TEXT_IO.ENUMERATION_IO(BOOLEAN);


  MM : constant := MAX_MEANING_SIZE;


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
    if S(S'FIRST) = '|'  then
      return TRIM(S(S'FIRST+1.. S'LAST));
    else
      return TRIM(S);
    end if;
  end TRIM_BAR;



  procedure PUT_CONSTRUCTED_MEANING_LINE(OUTPUT : TEXT_IO.FILE_TYPE;
                                      PR : in PARSE_RECORD) is
     N : INTEGER := 0;
  begin
     if PR.IR.QUAL.PART = NUM  then
        N := PR.IR.QUAL.NUM.VALUE;
        TEXT_IO.PUT(OUTPUT, INTEGER'IMAGE(N));
        case PR.IR.QUAL.NUM.KIND is
           when CARD    =>
              TEXT_IO.PUT(OUTPUT, " - (CARD answers 'how many')");
           when ORD     =>
              TEXT_IO.PUT(OUTPUT,
                          "th - (ORD, 'in series'); (a/the)" & INTEGER'IMAGE(N) &
                          "th (part) (fract w/pars?)");
           when DIST    =>
              TEXT_IO.PUT(OUTPUT,
                          " each/apiece/times/fold/together/at a time - 'how many each'; by " &
                          INTEGER'IMAGE(N) & "s ");
           when ADVERB  =>
              TEXT_IO.PUT(OUTPUT,
                          " times, on" & INTEGER'IMAGE(N) &
                          " occasions - (ADVERB answers 'how often')");
           when others  =>
              null;
        end case;
     end if;

     TEXT_IO.NEW_LINE(OUTPUT);

  end PUT_CONSTRUCTED_MEANING_LINE;



  procedure LIST_STEMS(OUTPUT : TEXT_IO.FILE_TYPE;
                       RAW_WORD : STRING;
                       PPA : in out PARSE_ARRAY; PPA_LAST : in out INTEGER) is
     use DICT_IO;

     subtype XONS is PART_OF_SPEECH_TYPE range TACKON..SUFFIX;

     J, J1, J2 : INTEGER := 0;
     DE, NEXT_DE : DICTIONARY_ENTRY;
     MEAN  : MEANING_TYPE := NULL_MEANING_TYPE;
     NEXT_MEAN  : MEANING_TYPE := NULL_MEANING_TYPE;
     MNPC : DICT_IO.COUNT;
     PR : PARSE_RECORD := NULL_PARSE_RECORD;
     OPR : PARSE_RECORD := NULL_PARSE_RECORD;
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
       if PR.IR.QUAL.PART not in XONS  then

         if WORDS_MODE(SHOW_AGE) and then
            (TRIM(DICTIONARY_AGE(PR.AAMNPC.AGE))'LENGTH /= 0)  then
            TEXT_IO.PUT(OUTPUT, " <" & TRIM(DICTIONARY_AGE(PR.AAMNPC.AGE)) & ">");
         end if;
         if WORDS_MODE(SHOW_FREQUENCY) and then
            (TRIM(DICTIONARY_FREQUENCY(PR.AAMNPC.FREQ))'LENGTH /= 0)  then
            TEXT_IO.PUT(OUTPUT, " <" & TRIM(DICTIONARY_FREQUENCY(PR.AAMNPC.FREQ)) & ">");
         end if;
       end if;
     end PUT_DICTIONARY_FLAGS;

     procedure PUT_DICTIONARY_FORM(OUTPUT : TEXT_IO.FILE_TYPE;
                                   DE : DICTIONARY_ENTRY;
                                   LINE_NUMBER : INTEGER := 0) is
        HIT : BOOLEAN := FALSE;
     begin
        if (PR.IR.QUAL.PART not in XONS)  and
           (PR.D_K in GENERAL..UNIQUE)           then

           if WORDS_MODE(DO_DICTIONARY_FORMS)  and
              (PR.D_K /= UNIQUE)                  then
              if DICTIONARY_FORM(DE, LINE_NUMBER)'LENGTH /= 0  then
                if WORDS_MDEV(DO_PEARSE_CODES) then
                  TEXT_IO.PUT(OUTPUT, "02 ");
                end if;
                TEXT_IO.PUT(OUTPUT, DICTIONARY_FORM(DE, LINE_NUMBER) & "  ");
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
           DE.PART.PART not in XONS              then
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
        if PPA(I).IR.QUAL.PART = ADV   then
           THERE_IS_AN_ADVERB := TRUE;
           exit;
        end if;
     end loop;


     if not THERE_IS_AN_ADVERB  then

        for I in reverse PPA'FIRST..PPA_LAST  loop

           if PPA(I).IR.QUAL.PART = ADJ and then
              (PPA(I).IR.QUAL.ADJ = ((1, 1), VOC, S, M, POS)    or
                  ((PPA(I).IR.QUAL.ADJ.CS = VOC)   and
                      (PPA(I).IR.QUAL.ADJ.NUMBER = S)   and
                      (PPA(I).IR.QUAL.ADJ.GENDER = M)   and
                      (PPA(I).IR.QUAL.ADJ.CO = SUPER)))    then

              J := I;

              while J >=  PPA'FIRST  loop  --Back through other ADJ cases
                 if PPA(J).IR.QUAL.PART /= ADJ  then
                    J2 := J;                          --  J2 is first (reverse) that is not ADJ
                    exit;
                 end if;
                 J := J - 1;
              end loop;
              while J >=  PPA'FIRST  loop  --  Sweep up associated fixes
                 if PPA(J).IR.QUAL.PART not in XONS  then
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
                                  PPP, NULL_AAMNPC_RECORD);
              PPA_LAST := PPA_LAST + 1;
              if PPA(J2+1).IR.QUAL.ADJ.CO = POS   then
                 PPA(PPA_LAST) := (PPA(J2+1).STEM,
                                        ((PART => ADV, ADV => (CO => PPA(J2+1).IR.QUAL.ADJ.CO)),
                                         KEY => 0, ENDING => (1, "e      "), AGE => X, FREQ => B),
                                     PPA(J2+1).D_K,
                                     PPA(J2+1).AAMNPC);
                 PPP_MEANING :=
                    HEAD("-ly; -ily;  Converting ADJ to ADV",
                         MAX_MEANING_SIZE);

              elsif PPA(J2+1).IR.QUAL.ADJ.CO = SUPER  then
                 PPA(PPA_LAST) := (PPA(J2+1).STEM,
                                        ((PART => ADV, ADV => (CO => PPA(J2+1).IR.QUAL.ADJ.CO)),
                                         KEY => 0, ENDING => (2, "me     "), AGE => X, FREQ => B),
                                     PPA(J2+1).D_K,
                                     PPA(J2+1).AAMNPC);
                 PPP_MEANING :=
                    HEAD("-estly; -estily; most -ly, very -ly  Converting ADJ to ADV",
                         MAX_MEANING_SIZE);
              end if;
           end if;           --  PPA(I).IR.QUAL.PART = ADJ

        end loop;

     end if;           --  not THERE_IS_AN_ADVERB


--TEXT_IO.NEW_LINE;
--TEXT_IO.PUT_LINE("Before SWEEPING");
--for I in 1..PPA_LAST  loop
--TEXT_IO.PUT(PPA(I).STEM & "  ");
--INFLECTION_RECORD_IO.PUT(PPA(I).IR);  TEXT_IO.NEW_LINE;
--end loop;


--    
--     SWEEPING:
--     --  To remove disallowed stems/inflections and resulting dangling fixes
--     declare
--        FIX_ON : BOOLEAN := FALSE;
--        PW_ON  : BOOLEAN := FALSE;
--        P_FIRST : INTEGER := 0;
--        P_LAST  : INTEGER := 0;
--     
--     begin
--     
----TEXT_IO.NEW_LINE;
----TEXT_IO.PUT_LINE("SWEEPING    ======================================");
----TEXT_IO.NEW_LINE;
--
--        J := PPA_LAST;
--     
--        while J >= 1  loop        --  Sweep backwards over PA
--        
----TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT("  =>   ");
----TEXT_IO.PUT_LINE(PPA(J).STEM);
----INFLECTION_RECORD_IO.PUT(TEXT_IO.STANDARD_OUTPUT, PPA(J).IR);
--        
--           if (not ALLOWED_STEM(PPA(J))   or       
--               (PPA(J) = NULL_PARSE_RECORD))  then
----TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT_LINE("  not ALLOWED");
--              PPA(J..PPA_LAST-1) := PPA(J+1..PPA_LAST);
--              PPA_LAST := PPA_LAST - 1;
--              P_LAST := P_LAST - 1;
----TEXT_IO.PUT_LINE(" -------------- " & INTEGER'IMAGE(PPA_LAST));
--           
--           elsif (PPA(J).D_K in ADDONS..YYY)   and then
--                 (PW_ON)     then               --  first FIX after regular
----TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT_LINE("  XONS/X  & PW_ON");
--              FIX_ON := TRUE;
----TEXT_IO.PUT_LINE("FIX_ON turned ON");
--                       PW_ON  := FALSE;
----TEXT_IO.PUT_LINE("PW_ON turned OFF");
--              P_FIRST := J + 1;
----TEXT_IO.PUT_LINE("                  " &  INTEGER'IMAGE(PPA_LAST));
--           
--           
--           --  Order internal to this set of inflections
--              ORDER_PARSE_ARRAY(PPA(P_FIRST..P_LAST));
--              P_FIRST := 0;
--              P_LAST  := 0;
--           
--
--           elsif (PPA(J).D_K in ADDONS..YYY)  and then
--                 (FIX_ON)     then               --  another FIX
----TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT("  XONS else");
----TEXT_IO.PUT_LINE("                 " & INTEGER'IMAGE(PPA_LAST));
--              null;
--           
--           elsif ((PPA(J).D_K in ADDONS..YYY)  or      
--                     (PPA(J).IR.QUAL.PART = X))  and then  --  Kills TRICKS stuff
--              (not PW_ON)     then 
----TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT_LINE("  XONS or X  and not PW_ON");
--              PPA(J..PPA_LAST-1) := PPA(J+1..PPA_LAST);
--              PPA_LAST := PPA_LAST - 1;
--              P_LAST := P_LAST - 1;
----TEXT_IO.PUT_LINE(" -------------- " & INTEGER'IMAGE(PPA_LAST));
--           
--           else 
----TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT_LINE("   else");
--              PW_ON := TRUE;
----TEXT_IO.PUT_LINE("PW_ON turned ON");
--              FIX_ON := FALSE;
----TEXT_IO.PUT_LINE("FIX_ON turned OFF");
--              if P_LAST <= 0  then
--                 P_LAST := J;
--              end if;
--              if J = 1  then
--                 ORDER_PARSE_ARRAY(PPA(1..P_LAST));
--              end if;
----TEXT_IO.PUT_LINE("                " & INTEGER'IMAGE(PPA_LAST));
--           
--           end if;                                      --  check PART
--           J := J - 1;
--        end loop;                          --  loop sweep over PA
--     
--     end SWEEPING;
--  
----TEXT_IO.PUT_LINE("PPA after SWEEPING  in LIST_STEMS - before COMPRESS_LOOP");
----for I in 1..PPA_LAST  loop
----PARSE_RECORD_IO.PUT(PPA(I)); TEXT_IO.NEW_LINE;
----end loop;
--  
--     OPR := NULL_PARSE_RECORD;
--  --  Last chance to weed out duplicates
--     J := 1;
--  COMPRESS_LOOP:
--     loop
--        exit when J > PPA_LAST;
--        PR := PPA(J);
--        if PR /= OPR  then
--           SUPRESS_KEY_CHECK:
--           declare
--              function "<=" (A, B : PARSE_RECORD) return BOOLEAN is
--              begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
--                 if A.IR.QUAL = B.IR.QUAL  and 
--                 A.AAMNPC.MNPC    = B.AAMNPC.MNPC     then
--                    return TRUE;
--                 else 
--                    return FALSE;
--                 end if;
--              end "<=";
--           begin
--              if ((PR.D_K /= XXX) and (PR.D_K /= YYY) and  (PR.D_K /= PPP)) and then
--              PR <= OPR  then       --  Get rid of duplicates, if ORDER is OK
--                 PPA(J.. PPA_LAST-1) := PPA(J+1..PPA_LAST);  --  Shift PPA down 1
--                 PPA_LAST := PPA_LAST - 1;        --  because found key duplicate
--              else
--                 J := J + 1;
--              end if;
--           end SUPRESS_KEY_CHECK;
--        else
--           PPA(J.. PPA_LAST-1) := PPA(J+1..PPA_LAST);  --  Shift PPA down 1
--           PPA_LAST := PPA_LAST - 1;            --  because found exact duplicate
--        end if;
--        OPR := PR;
--     end loop COMPRESS_LOOP;

--TEXT_IO.PUT_LINE("PPA after COMPRESS_LOOP - before OUTPUT_LOOP");
--for I in 1..PPA_LAST  loop
--PARSE_RECORD_IO.PUT(PPA(I)); TEXT_IO.NEW_LINE;
--end loop;




LIST_LIST(PPA(1..PPA_LAST));





     OPR := NULL_PARSE_RECORD;
  OUTPUT_LOOP:
     for I in 1..PPA_LAST  loop
--TEXT_IO.PUT_LINE("OUTPUT LOOP    I = " & INTEGER'IMAGE(I));
        PR := PPA(I);


     --  Put each remaining parse - stem, ending, then inflection part record
        PUT_INFLECTION:
        begin
           if (PR /= OPR  and then (not WORDS_MODE(DO_ONLY_MEANINGS) and
                                   not (CONFIGURATION = MEANINGS)))  and then
               PR /= NULL_PARSE_RECORD    then   --  From trimming Archaic/Medieval
              TEXT_IO.SET_COL(OUTPUT, 1);
              if WORDS_MDEV(DO_PEARSE_CODES) then
                if PR.D_K = ADDONS  then
                  TEXT_IO.PUT(OUTPUT, "05 ");
                elsif PR.D_K in XXX..PPP  then
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
                 PUT_EXAMPLE_LINE(OUTPUT, PR);    --  Only full when DO_EXAMPLES
              else
                 TEXT_IO.NEW_LINE(OUTPUT);
              end if;
           end if;
        end PUT_INFLECTION;


        OPR := PR;


       --  Maybe I need to put all inflections then forms then meanings\
       --  Maybe I cannot just skip until
       --  Maybe I could put to a buffer , then clear it/them when ready to print
       --  Should this be done in the LISTLIST, arranging the PRs and sorting 

        PUT_FORM: begin
           MNPC := PR.AAMNPC.MNPC;
           if PR.D_K not in XXX..PPP then
           --  Check to see if the dictionary entry has changed and so should be put
             if I = PPA_LAST    or else PPA(I+1).AAMNPC.MNPC /= MNPC  then    --   |||||??? --  Here could check if meanings are the same but form will be different
--TEXT_IO.PUT_LINE("I = " & INTEGER'IMAGE(I) & "   FORM");                    --  Want situation where forms are same but meaning different
               DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PR.AAMNPC.MNPC);          --  Need to make sure that TRANS is exactly the same for multiline meanings
               DICT_IO.READ(DICT_FILE(PR.D_K), DE);
               LINE_NUMBER := INTEGER(PR.AAMNPC.MNPC);
               --PUT_DICTIONARY_FLAGS;
               PUT_DICTIONARY_FORM(OUTPUT, DE, LINE_NUMBER);
             end if;
           end if;
        end PUT_FORM;


        PUT_MEANING: begin
           MNPC := PR.AAMNPC.MNPC;
        --  Check to see if the meaning has changed and so should be put
           if I = PPA_LAST    or else
           PPA(I+1).AAMNPC.MNPC /= MNPC  or else
           PPA(I+1).D_K /= PPA(I).D_K  then
           --  Even if MNPC different, the MEAN might be identical  
              if PR.D_K not in XXX..PPP then
                 DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PR.AAMNPC.MNPC);
                 DICT_IO.READ(DICT_FILE(PR.D_K), DE);
                 MEAN := DE.TRAN.MEAN;
                 if I < PPA_LAST and then PPA(I+1).D_K = PR.D_K  then
                    DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PPA(I+1).AAMNPC.MNPC);
                    DICT_IO.READ(DICT_FILE(PR.D_K), NEXT_DE);
                    NEXT_MEAN := NEXT_DE.TRAN.MEAN;
                    if MEAN /= NEXT_MEAN  then
                      if WORDS_MDEV(DO_PEARSE_CODES) then
                        TEXT_IO.PUT(OUTPUT, "03 ");
                      end if;
                      if PR.IR.QUAL.PART = NUM  and then PR.IR.QUAL.NUM.VALUE > 3 then
                          PUT_CONSTRUCTED_MEANING_LINE(OUTPUT, PR);    --  Constructed MEANING
                       else
                          TEXT_IO.PUT_LINE(OUTPUT, TRIM_BAR(MEAN));
                       end if;
                    end if;
                 else
                    if WORDS_MDEV(DO_PEARSE_CODES) then
                        TEXT_IO.PUT(OUTPUT, "03 ");
                    end if;
                    if PR.IR.QUAL.PART = NUM  and then PR.IR.QUAL.NUM.VALUE > 3 then
                       PUT_CONSTRUCTED_MEANING_LINE(OUTPUT, PR);    --  Constructed MEANING
                    else
                       TEXT_IO.PUT_LINE(OUTPUT, TRIM_BAR(MEAN));
                    end if;
                 end if;
              else
                 if WORDS_MDEV(DO_PEARSE_CODES) then
                   TEXT_IO.PUT(OUTPUT, "03 ");
                 end if;
                 if PR.D_K = RRR and then PR.IR.QUAL.PART = NUM then
                    PUT_DICTIONARY_FLAGS;
                    TEXT_IO.PUT(OUTPUT, INTEGER'IMAGE(INTEGER(PR.IR.QUAL.NUM.VALUE)) &
                                "  as a ROMAN NUMERAL");
                    TEXT_IO.NEW_LINE(OUTPUT);
                 elsif PR.D_K = NNN and then NNN_MEANING /= NULL_MEANING_TYPE  then
                    PUT_DICTIONARY_FLAGS;
                    TEXT_IO.PUT_LINE(OUTPUT, TRIM(NNN_MEANING));
                    NNN_MEANING := NULL_MEANING_TYPE;
                 elsif PR.D_K = XXX and then XXX_MEANING /= NULL_MEANING_TYPE  then
                    PUT_DICTIONARY_FLAGS;
                    TEXT_IO.PUT_LINE(OUTPUT, TRIM(XXX_MEANING));
                    XXX_MEANING := NULL_MEANING_TYPE;
                 elsif PR.D_K = YYY and then YYY_MEANING /= NULL_MEANING_TYPE  then
                    PUT_DICTIONARY_FLAGS;
                    TEXT_IO.PUT_LINE(OUTPUT, TRIM(YYY_MEANING));
                    YYY_MEANING := NULL_MEANING_TYPE;
                 elsif PR.D_K = PPP and then PPP_MEANING /= NULL_MEANING_TYPE  then
                    PUT_DICTIONARY_FLAGS;
                    TEXT_IO.PUT_LINE(OUTPUT, TRIM(PPP_MEANING));
                    PPP_MEANING := NULL_MEANING_TYPE;
                 end if;
              end if;


           end if;
        end PUT_MEANING;

--TEXT_IO.PUT_LINE("SCROLL LINE NUMBER = " & INTEGER'IMAGE(SCROLL_LINE_NUMBER) & 
--"   OUTPUT_SCREEN_SIZE = " & INTEGER'IMAGE(OUTPUT_SCREEN_SIZE) &
--"   LINE  = " & INTEGER'IMAGE(INTEGER(TEXT_IO.LINE(TEXT_IO.STANDARD_OUTPUT))));
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


     end loop OUTPUT_LOOP;

 --TEXT_IO.PUT_LINE("End of LIST_STEMS");
  end LIST_STEMS;


---------  Now do the listing of stems and inflctions when unknown  -------



  procedure WEED_PDL is
     J : INTEGER := PDL_INDEX;
     J_LAST : INTEGER := PDL_INDEX;


     function ALLOWED_PD(PD : PRUNED_DICTIONARY_ITEM) return BOOLEAN is
     --use LATIN_DEBUG;
     begin

        if PD.DS.PART.PART = N  then            --  if NOUN
           if PD.DS.PART.N.DECL = (9, 8)  or       --  ignore  abbr
           PD.DS.PART.N.DECL = (9, 9)  then     --  ignore  undeclined
              return FALSE;
           else
              return TRUE;
           end if;

        elsif PD.DS.PART.PART = ADJ  then           --  if ADJ
           if PD.DS.PART.ADJ.DECL = (9, 8)  or       --  ignore  abbr
           PD.DS.PART.ADJ.DECL = (9, 9)  then     --  ignore  undeclined
              return FALSE;
           else
              return TRUE;
           end if;


        elsif PD.DS.PART.PART = V  then             --  if VERB
           if PD.DS.PART.V.CON.WHICH > 4  then     --  ignore irregulars
              return FALSE;
           elsif PD.DS.PART.V.KIND = IMPERS  then
              return FALSE;
           elsif (PD.DS.KEY = 1)  or                 --  only KEY 2 or 4
              (PD.DS.KEY = 3)  then
              return FALSE;
           else
              return TRUE;
           end if;

        else                                       --  All others
           return FALSE;
        end if;

        return FALSE;

     end ALLOWED_PD;

  begin
     while J > 0  loop
        if not ALLOWED_PD(PDL(J))  then
           PDL(J..J_LAST-1) := PDL(J+1..J_LAST);
           PDL(J_LAST) := NULL_PRUNED_DICTIONARY_ITEM;
           J_LAST := J_LAST - 1;
        end if;
        J := J - 1;
     end loop;
     PDL_INDEX := J_LAST;
  end WEED_PDL;


  function ALLOWED_INFLECTION(PR : PARSE_RECORD) return BOOLEAN is
  --      use LATIN_DEBUG;
  begin
     if PR.IR.ENDING.SIZE = 0  then
        return FALSE;
     end if;

     if PR.IR.QUAL.PART = N  then            --  if NOUN
        if (PR.IR.QUAL.N.CS = VOC) or else
           (PR.IR.QUAL.N.CS = LOC) then
           return FALSE;
        elsif PR.IR.QUAL.N.DECL.VAR   > 5  then     --  ignore Greek DECL
           return FALSE;
        else
           return TRUE;
        end if;

     elsif PR.IR.QUAL.PART = ADJ  then           --  if ADJ
        if (PR.IR.QUAL.ADJ.CS = VOC) then
           return FALSE;
        elsif (PR.IR.QUAL.ADJ.CS = LOC) then
           return FALSE;
        else
           return TRUE;
        end if;

     elsif PR.IR.QUAL.PART = V  then             --  if VERB
        if PR.IR.QUAL.V.CON.WHICH < 4  then
           return TRUE;
        else
           return FALSE;                         --  ignore most irregulars
        end if;

     elsif PR.IR.QUAL.PART = VPAR  then             --  if VERB PPL
        if PR.IR.QUAL.VPAR.CON.WHICH < 4  or else
        PR.IR.QUAL.VPAR.CON  = (6, 1)  then
           return TRUE;
        else
           return FALSE;                         --  ignore most irregulars
        end if;

     else                                       --  All others
        return FALSE;
     end if;

     return FALSE;

  end ALLOWED_INFLECTION;


  procedure WEED_INFLECTIONS(SS : in out PARSE_ARRAY) is
     J : INTEGER := SS'LAST;
     J_LAST : INTEGER := SS'LAST;
  begin
     while J > 0  loop
        if not ALLOWED_INFLECTION(SS(J))  then
           SS(J..J_LAST-1) := SS(J+1..J_LAST);
           SS(J_LAST) := NULL_PARSE_RECORD;
           J_LAST := J_LAST - 1;
        end if;
        J := J - 1;
     end loop;
  end WEED_INFLECTIONS;


  function NO_DIFFERENCE(PR, PS : PARSE_RECORD) return BOOLEAN is
  --      use LATIN_DEBUG;
  begin
     if PR.IR.QUAL.PART /= PS.IR.QUAL.PART  then
        return FALSE;
     else

        if PR.IR.QUAL.PART = N  then            --  if NOUN
           if (PR.IR.QUAL.N.CS = PS.IR.QUAL.N.CS) and then
              (PR.IR.QUAL.N.NUMBER = PS.IR.QUAL.N.NUMBER) and then
              (PR.IR.QUAL.N.GENDER = PS.IR.QUAL.N.GENDER)     then
              return TRUE;
           else
              return FALSE;
           end if;


        elsif PR.IR.QUAL.PART = ADJ  then           --  if ADJ
           if (PR.IR.QUAL.ADJ.CS = PS.IR.QUAL.ADJ.CS) and then
              (PR.IR.QUAL.ADJ.NUMBER = PS.IR.QUAL.ADJ.NUMBER) and then
              (PR.IR.QUAL.ADJ.GENDER = PS.IR.QUAL.ADJ.GENDER)     then
              return TRUE;
           else
              return FALSE;
           end if;

        elsif PR.IR.QUAL.PART = V  then             --  if VERB
           if (PR.IR.QUAL.V.TENSE_VOICE_MOOD = PS.IR.QUAL.V.TENSE_VOICE_MOOD) and then
              (PR.IR.QUAL.V.NUMBER = PS.IR.QUAL.V.NUMBER) and then
              (PR.IR.QUAL.V.PERSON = PS.IR.QUAL.V.PERSON)     then
              return TRUE;
           else
              return FALSE;
           end if;

        elsif PR.IR.QUAL.PART = VPAR  then             --  if VPAR
           if (PR.IR.QUAL.VPAR.TENSE_VOICE_MOOD =
               PS.IR.QUAL.VPAR.TENSE_VOICE_MOOD) and then
              (PR.IR.QUAL.VPAR.NUMBER = PS.IR.QUAL.VPAR.NUMBER) and then
              (PR.IR.QUAL.VPAR.GENDER = PS.IR.QUAL.VPAR.GENDER)     then
              return TRUE;
           else
              return FALSE;
           end if;

        else                                       --  All others
           return  FALSE;
        end if;

     end if;

  end NO_DIFFERENCE;

  procedure COMPRESS_INFLECTIONS(SS : in out PARSE_ARRAY) is
  --    use LATIN_DEBUG;
     J : INTEGER := SS'LAST;
     J_LAST : INTEGER := J;
  begin
  --LATIN_DEBUG.PUT_LINE("COMPRESSING    J_LAST starts at " & INTEGER'IMAGE(J_LAST));
     while J > 0  loop
        if SS(J) = NULL_PARSE_RECORD  then
           SS(J_LAST) := NULL_PARSE_RECORD;
           J_LAST := J_LAST - 1;

        elsif NO_DIFFERENCE(SS(J), SS(J+1))  then
           SS(J..J_LAST-1) := SS(J+1..J_LAST);
           SS(J_LAST) := NULL_PARSE_RECORD;
           J_LAST := J_LAST - 1;

        else
        --  Compressing GENDER

           if ((SS(J).IR.QUAL.PART = N) and (SS(J+1).IR.QUAL.PART = N))  and then
              ((SS(J).IR.QUAL.N.CS = SS(J+1).IR.QUAL.N.CS)) and then
              ((SS(J).IR.QUAL.N.NUMBER = SS(J+1).IR.QUAL.N.NUMBER))     then

              if (SS(J).IR.QUAL.N.GENDER = X)  or else
                 (SS(J+1).IR.QUAL.N.GENDER = X)    then
                 SS(J+1).IR.QUAL.N.GENDER := X;
                 SS(J..J_LAST-1) := SS(J+1..J_LAST);
                 SS(J_LAST) := NULL_PARSE_RECORD;
                 J_LAST := J_LAST - 1;

              elsif ((SS(J).IR.QUAL.N.GENDER = C)  and then
                        ((SS(J+1).IR.QUAL.N.GENDER = M)   or
                            (SS(J+1).IR.QUAL.N.GENDER = F)))   or else
                 ((SS(J+1).IR.QUAL.N.GENDER = C)  and then
                     ((SS(J).IR.QUAL.N.GENDER = M)   or
                         (SS(J).IR.QUAL.N.GENDER = F)))  then
                 SS(J+1).IR.QUAL.N.GENDER := C;
                 SS(J..J_LAST-1) := SS(J+1..J_LAST);
                 SS(J_LAST) := NULL_PARSE_RECORD;
                 J_LAST := J_LAST - 1;

              elsif ((SS(J).IR.QUAL.N.GENDER = C)  and then
                        ((SS(J+1).IR.QUAL.N.GENDER = N)))   or else
                 ((SS(J+1).IR.QUAL.N.GENDER = C)  and then
                     ((SS(J).IR.QUAL.N.GENDER = N)))  then
                 SS(J+1).IR.QUAL.N.GENDER := X;
                 SS(J..J_LAST-1) := SS(J+1..J_LAST);
                 SS(J_LAST) := NULL_PARSE_RECORD;
                 J_LAST := J_LAST - 1;

              end if;


           elsif ((SS(J).IR.QUAL.PART = ADJ) and (SS(J+1).IR.QUAL.PART = ADJ))  and then
              ((SS(J).IR.QUAL.ADJ.CS = SS(J+1).IR.QUAL.ADJ.CS)) and then
              ((SS(J).IR.QUAL.ADJ.NUMBER = SS(J+1).IR.QUAL.ADJ.NUMBER))  then

              if (SS(J).IR.QUAL.ADJ.GENDER = X)  or else
                 (SS(J+1).IR.QUAL.ADJ.GENDER = X)    then
                 SS(J+1).IR.QUAL.ADJ.GENDER := X;
                 SS(J..J_LAST-1) := SS(J+1..J_LAST);
                 SS(J_LAST) := NULL_PARSE_RECORD;
                 J_LAST := J_LAST - 1;

              elsif ((SS(J).IR.QUAL.ADJ.GENDER = C)  and then
                        ((SS(J+1).IR.QUAL.ADJ.GENDER = M)   or
                            (SS(J+1).IR.QUAL.ADJ.GENDER = F)))   or else
                 ((SS(J+1).IR.QUAL.ADJ.GENDER = C)  and then
                     ((SS(J).IR.QUAL.ADJ.GENDER = M)   or
                         (SS(J).IR.QUAL.ADJ.GENDER = F)))  then
                 SS(J+1).IR.QUAL.ADJ.GENDER := C;
                 SS(J..J_LAST-1) := SS(J+1..J_LAST);
                 SS(J_LAST) := NULL_PARSE_RECORD;
                 J_LAST := J_LAST - 1;

              elsif ((SS(J).IR.QUAL.ADJ.GENDER = C)  and then
                        ((SS(J+1).IR.QUAL.ADJ.GENDER = N)))   or else
                 ((SS(J+1).IR.QUAL.ADJ.GENDER = C)  and then
                     ((SS(J).IR.QUAL.ADJ.GENDER = N)))  then
                 SS(J+1).IR.QUAL.ADJ.GENDER := X;
                 SS(J..J_LAST-1) := SS(J+1..J_LAST);
                 SS(J_LAST) := NULL_PARSE_RECORD;
                 J_LAST := J_LAST - 1;

              end if;

           end if;

        end if;
        J := J - 1;
     end loop;
  --LATIN_DEBUG.PUT_LINE("COMPRESSED down to  " & INTEGER'IMAGE(J_LAST));
  end COMPRESS_INFLECTIONS;





  procedure ORDER_INFLECTIONS(SX : in out PARSE_ARRAY) is
     use INFLECTION_RECORD_IO;
     use DICT_IO;
  --    use LATIN_DEBUG;

     HITS : INTEGER := 0;
     SL : PARSE_ARRAY(SX'FIRST..SX'LAST) := SX;
     SM : PARSE_RECORD;
     I : INTEGER := 0;

     function "<"(LEFT, RIGHT : TENSE_VOICE_MOOD_RECORD) return BOOLEAN is
     begin
        if LEFT.MOOD < RIGHT.MOOD    then
           return TRUE;


        elsif (LEFT.MOOD = RIGHT.MOOD   and then
               LEFT.VOICE < RIGHT.VOICE)    then
           return TRUE;


        elsif (LEFT.MOOD = RIGHT.MOOD   and then
               LEFT.VOICE = RIGHT.VOICE  and then
               LEFT.TENSE < RIGHT.TENSE)     then

           return TRUE;
        else
           return FALSE;
        end if;
        return FALSE;
     end "<";


     function "<" (LEFT, RIGHT : INFLECTION_RECORD) return BOOLEAN is
     --  For LIST_INF only
     begin
        if (LEFT.ENDING.SIZE < RIGHT.ENDING.SIZE)  then

           return TRUE;


        elsif (LEFT.ENDING.SIZE = RIGHT.ENDING.SIZE  and then
               LEFT.QUAL.PART < RIGHT.QUAL.PART)            then

           return TRUE;

        elsif (LEFT.ENDING.SIZE = RIGHT.ENDING.SIZE  and then
               LEFT.QUAL.PART = RIGHT.QUAL.PART)               then

           if (LEFT.QUAL.PART = N       and then

                  ((LEFT.QUAL.N.CS < RIGHT.QUAL.N.CS)  or else

                      (LEFT.QUAL.N.CS = RIGHT.QUAL.N.CS  and then
                       LEFT.QUAL.N.NUMBER < RIGHT.QUAL.N.NUMBER)  or else

                      (LEFT.QUAL.N.CS = RIGHT.QUAL.N.CS   and then
                       LEFT.QUAL.N.NUMBER = RIGHT.QUAL.N.NUMBER   and then
                       LEFT.QUAL.N.GENDER < RIGHT.QUAL.N.GENDER)))    then

              return TRUE;

           elsif (LEFT.QUAL.PART = ADJ  and then

                     ((LEFT.QUAL.ADJ.CS < RIGHT.QUAL.ADJ.CS)  or else

                         (LEFT.QUAL.ADJ.CS = RIGHT.QUAL.ADJ.CS  and then
                          LEFT.QUAL.ADJ.NUMBER < RIGHT.QUAL.ADJ.NUMBER)  or else

                         (LEFT.QUAL.ADJ.CS = RIGHT.QUAL.ADJ.CS   and then
                          LEFT.QUAL.ADJ.NUMBER = RIGHT.QUAL.ADJ.NUMBER   and then
                          LEFT.QUAL.ADJ.GENDER < RIGHT.QUAL.ADJ.GENDER)))    then

              return TRUE;

           elsif (LEFT.QUAL.PART = V    and then

                     ((LEFT.QUAL.V.TENSE_VOICE_MOOD < RIGHT.QUAL.V.TENSE_VOICE_MOOD) or else

                         (LEFT.QUAL.V.TENSE_VOICE_MOOD = RIGHT.QUAL.V.TENSE_VOICE_MOOD  and then
                          LEFT.QUAL.V.NUMBER < RIGHT.QUAL.V.NUMBER)  or else

                         (LEFT.QUAL.V.TENSE_VOICE_MOOD = RIGHT.QUAL.V.TENSE_VOICE_MOOD  and then
                          LEFT.QUAL.V.NUMBER = RIGHT.QUAL.V.NUMBER   and then
                          LEFT.QUAL.V.PERSON < RIGHT.QUAL.V.PERSON)))    then

              return TRUE;

           elsif (LEFT.QUAL.PART = VPAR  and then

                     ((LEFT.QUAL.VPAR.TENSE_VOICE_MOOD < RIGHT.QUAL.VPAR.TENSE_VOICE_MOOD) or else

                         (LEFT.QUAL.VPAR.TENSE_VOICE_MOOD < RIGHT.QUAL.VPAR.TENSE_VOICE_MOOD  and then
                          LEFT.QUAL.VPAR.CS < RIGHT.QUAL.VPAR.CS)  or else

                         (LEFT.QUAL.VPAR.TENSE_VOICE_MOOD < RIGHT.QUAL.VPAR.TENSE_VOICE_MOOD  and then
                          LEFT.QUAL.VPAR.CS = RIGHT.QUAL.VPAR.CS  and then
                          LEFT.QUAL.VPAR.NUMBER < RIGHT.QUAL.VPAR.NUMBER)  or else

                         (LEFT.QUAL.VPAR.TENSE_VOICE_MOOD < RIGHT.QUAL.VPAR.TENSE_VOICE_MOOD  and then
                          LEFT.QUAL.VPAR.CS = RIGHT.QUAL.VPAR.CS   and then
                          LEFT.QUAL.VPAR.NUMBER = RIGHT.QUAL.VPAR.NUMBER   and then
                          LEFT.QUAL.VPAR.GENDER < RIGHT.QUAL.VPAR.GENDER)))    then

              return TRUE;

           else
              return FALSE;
           end if;
           return FALSE;
        else
           return FALSE;
        end if;

     end "<";

  begin
  --LATIN_DEBUG.PUT_LINE("In ORDER_INFLECTIONS");
     if SX'LENGTH = 0              then
        return;
     end if;


  --  Bubble sort                                                         
  --LATIN_DEBUG.PUT_LINE("HIT_LOOP:           ");
  HIT_LOOP:
     loop
        HITS := 0;


     --------------------------------------------------


        SWITCH:
        begin
        --  Need to remove duplicates in ARRAY_STEMS
        --  This sort is very sloppy



           I := 1;
        INNER_LOOP:
        --for I in SL'FIRST..SL'LAST-1  loop
           while SL(I+1) /= NULL_PARSE_RECORD  loop
              if SL(I+1).IR < SL(I).IR      then
                 SM := SL(I);
                 SL(I) := SL(I+1);
                 SL(I+1) := SM;
                 HITS := HITS + 1;
              end if;
              I := I + 1;
           end loop INNER_LOOP;

        end SWITCH;

        exit when HITS = 0;
     end loop HIT_LOOP;
     SX := SL;
  end ORDER_INFLECTIONS;

--==========================================================================

--  procedure LIST_POSSIBLES(OUTPUT : TEXT_IO.FILE_TYPE; INPUT_WORD : STRING) is
--  begin
--    null;
--  end LIST_POSSIBLES;


  procedure LIST_POSSIBLES(OUTPUT : TEXT_IO.FILE_TYPE; INPUT_WORD : STRING) is
     use TEXT_IO;
     use PART_OF_SPEECH_TYPE_IO;
     use PART_ENTRY_IO;
     use PARSE_RECORD_IO;
     use QUALITY_RECORD_IO;
     use ENDING_RECORD_IO;
     use INFLECTIONS_PACKAGE.INTEGER_IO;
     use CASE_TYPE_IO;
     use PERSON_TYPE_IO;
     use NUMBER_TYPE_IO;
     use GENDER_TYPE_IO;
     use TENSE_VOICE_MOOD_RECORD_IO;
     use DICT_IO;

     SSS : SAL;
     PD : PRUNED_DICTIONARY_ITEM := NULL_PRUNED_DICTIONARY_ITEM;
     DE : DICTIONARY_ENTRY;
     MEAN : MEANING_TYPE := NULL_MEANING_TYPE;

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

  begin

     PUT_LINE(OUTPUT,
              "Possible Dictionary Stems and Some Inflections (N, ADJ, V, PPL)");

  --  Fill up the SSA to catch all possible stems
     GENERATE_FULL_STEM_ARRAY:
     declare
        J : INTEGER := 1;
     begin
     --  At this point we ignore Z = 0 (esse, KEY = 2), the blank stem
     --  Probably ought to ignore Z = 1 also
        for Z in 1..MIN(MAX_STEM_SIZE, LEN(INPUT_WORD))  loop
           SSA(J) := NULL_STEM_TYPE;
           SSA(J)(1..Z) := INPUT_WORD(INPUT_WORD'FIRST..INPUT_WORD'FIRST+Z-1);
           SSA_MAX := J;
           J := J + 1;
        end loop;
     end GENERATE_FULL_STEM_ARRAY;

     SEARCH_DICTIONARIES(SSA(1..SSA_MAX), NULL_PREFIX_ITEM, NULL_SUFFIX_ITEM);

  --  WEED PDL and SORT on MNPC
  --  Ignore sort for the moment

     WEED_PDL;



     if PDL_INDEX <= 0  then
        PUT_LINE(OUTPUT, "========   No useful stems exist in the dictionaries ");

     elsif PDL_INDEX = 1  then

        PUT(OUTPUT, "    " & PDL(1).DS.STEM);
        PUT(OUTPUT, PDL(1).DS.PART.PART); NEW_LINE(OUTPUT);

        DICT_IO.SET_INDEX(DICT_FILE(PDL(1).D_K), PDL(1).DS.AAMNPC.MNPC);
        DICT_IO.READ(DICT_FILE(PDL(1).D_K), DE);
        MEAN := DE.TRAN.MEAN;
        PUT(OUTPUT,  EXT(PDL(1).D_K) & "> ");
     --PUT_LINE(OUTPUT, MEAN(1..MIN(MAX_MEANING_SIZE, MM)));
        TEXT_IO.PUT_LINE(OUTPUT, TRIM(MEAN));



     elsif PDL_INDEX > 1  then
        PD := PDL(1);

        for J in 1..PDL_INDEX  loop

           if J = PDL_INDEX              then

              if ((PDL(J).DS.AAMNPC.MNPC /= PD.DS.AAMNPC.MNPC)  or
                     (PDL(J).D_K /= PD.D_K))   then
                 DICT_IO.SET_INDEX(DICT_FILE(PD.D_K), PD.DS.AAMNPC.MNPC);
                 DICT_IO.READ(DICT_FILE(PD.D_K), DE);
                 MEAN := DE.TRAN.MEAN;
                 PUT(OUTPUT, EXT(PD.D_K) & "> ");
              --PUT_LINE(OUTPUT, MEAN(1..MIN(MAX_MEANING_SIZE, MM)));
                 TEXT_IO.PUT_LINE(OUTPUT, TRIM(MEAN));

                 PUT(OUTPUT, "    " & PDL(J).DS.STEM);
                 PUT(OUTPUT, PDL(J).DS.PART.PART); NEW_LINE(OUTPUT);

              elsif (PDL(J).DS.STEM /= PD.DS.STEM)    then
                 PUT(OUTPUT, "    " & PDL(J).DS.STEM);
                 PUT(OUTPUT, PDL(J).DS.PART.PART); NEW_LINE(OUTPUT);
              end if;


              DICT_IO.SET_INDEX(DICT_FILE(PDL(J).D_K), PDL(J).DS.AAMNPC.MNPC);
              DICT_IO.READ(DICT_FILE(PDL(J).D_K), DE);
              MEAN := DE.TRAN.MEAN;
              PUT(OUTPUT, EXT(PDL(J).D_K) & "> ");
           --PUT_LINE(OUTPUT, MEAN(1..MIN(MAX_MEANING_SIZE, MM)));
              TEXT_IO.PUT_LINE(OUTPUT, TRIM(MEAN));

              exit;

           elsif J = 1                      then

              PUT(OUTPUT, "    " & PDL(J).DS.STEM);
              PUT(OUTPUT, PDL(J).DS.PART.PART); NEW_LINE(OUTPUT);

              PD := PDL(J);


           elsif ((PDL(J).DS.AAMNPC.MNPC = PD.DS.AAMNPC.MNPC)  and  (PDL(J).D_K = PD.D_K))   then
           --  skip

              PD := PDL(J);

           else

              DICT_IO.SET_INDEX(DICT_FILE(PD.D_K), PD.DS.AAMNPC.MNPC);
              DICT_IO.READ(DICT_FILE(PD.D_K), DE);
              MEAN := DE.TRAN.MEAN;
              PUT(OUTPUT, EXT(PD.D_K) & "> ");
           --PUT_LINE(OUTPUT, MEAN(1..MIN(MAX_MEANING_SIZE, MM)));
              TEXT_IO.PUT_LINE(OUTPUT, TRIM(MEAN));

              PUT(OUTPUT, "    " & PDL(J).DS.STEM);
              PUT(OUTPUT, PDL(J).DS.PART.PART); NEW_LINE(OUTPUT);

              PD := PDL(J);

           end if;

          DO_STEM_PAUSE_HERE:
          begin
                if  (INTEGER(TEXT_IO.LINE(OUTPUT)) >
                       SCROLL_LINE_NUMBER + OUTPUT_SCREEN_SIZE)  then
                  PAUSE;
                  SCROLL_LINE_NUMBER := INTEGER(TEXT_IO.LINE(OUTPUT));
                end if;
              end DO_STEM_PAUSE_HERE;

        end loop;

     end if;


     RUN_INFLECTIONS(INPUT_WORD, SSS);
     WEED_INFLECTIONS(SSS);
     ORDER_INFLECTIONS(SSS);
     COMPRESS_INFLECTIONS(SSS);
     for I in SSS'RANGE loop
        if SSS(I) /= NULL_PARSE_RECORD  then
           PUT(OUTPUT, "    "); PUT(OUTPUT, SSS(I).IR.ENDING.SUF);
           SET_COL(OUTPUT, 12);

           if SSS(I).IR.QUAL.PART = N  then
              PUT(OUTPUT, SSS(I).IR.QUAL.PART); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.N.CS); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.N.NUMBER); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.N.GENDER); PUT(OUTPUT, "  ");

           elsif SSS(I).IR.QUAL.PART = ADJ  then
              PUT(OUTPUT, SSS(I).IR.QUAL.PART); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.ADJ.CS); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.ADJ.NUMBER); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.ADJ.GENDER); PUT(OUTPUT, "  ");

           elsif SSS(I).IR.QUAL.PART = V  then
              PUT(OUTPUT, SSS(I).IR.QUAL.PART); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.V.TENSE_VOICE_MOOD); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.V.PERSON); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.V.NUMBER); PUT(OUTPUT, "  ");

           elsif SSS(I).IR.QUAL.PART = VPAR  then
              PUT(OUTPUT, SSS(I).IR.QUAL.PART); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.VPAR.CS); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.VPAR.NUMBER); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.VPAR.GENDER); PUT(OUTPUT, "  ");
              PUT(OUTPUT, SSS(I).IR.QUAL.VPAR.TENSE_VOICE_MOOD); PUT(OUTPUT, "  ");

           end if;
           NEW_LINE(OUTPUT);
        end if;


        DO_INFL_PAUSE_HERE:
          begin
                if  (INTEGER(TEXT_IO.LINE(OUTPUT)) >
                       SCROLL_LINE_NUMBER + OUTPUT_SCREEN_SIZE)  then
                  PAUSE;
                  SCROLL_LINE_NUMBER := INTEGER(TEXT_IO.LINE(OUTPUT));
                end if;
              end DO_INFL_PAUSE_HERE;

     end loop;

  end LIST_POSSIBLES;

--------------------------------------------------------------------

end LIST_PACKAGE;