with TEXT_IO; 
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with CONFIG; use CONFIG;
with WORD_PARAMETERS; use WORD_PARAMETERS;
--with LATIN_DEBUG; 
procedure PUT_EXAMPLE_LINE(OUTPUT : TEXT_IO.FILE_TYPE; PR : in PARSE_RECORD; KE : in KIND_ENTRY) is
--      use LATIN_DEBUG;

    procedure PUT_VERB_EXAMPLE(OUTPUT : TEXT_IO.FILE_TYPE; PR : in PARSE_RECORD; KE : in KIND_ENTRY) is
      PERSON : constant PERSON_TYPE      := PR.IR.QUAL.V.PERSON;
      NUMBER : constant NUMBER_TYPE      := PR.IR.QUAL.V.NUMBER;
      TENSE  : constant TENSE_TYPE       := PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE;
      MOOD   : constant MOOD_TYPE        := PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD; 
      VOICE  : VOICE_TYPE       := PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE;
      KIND   : VERB_KIND_TYPE   := KE.V_KIND; 
--  Nothing on  (part), gerund, 

      function THEY return STRING is
      begin
        if KIND = IMPERS  then
          return "it ";
        end if;

        if MOOD = INF then
          return "to ";
        end if;

        if MOOD = IMP and TENSE = PRES  then
          return "";
        end if;
        
        if  NUMBER = S  then
          if PERSON = 1  then
            return "I ";
          elsif  PERSON = 2  then
            return "you ";
          elsif  PERSON = 3  then
            return "he/it ";
          else
            return "";
          end if;
        elsif NUMBER = P  then
          if PERSON = 1  then
            return "we ";
          elsif  PERSON = 2  then
            return "you ";
          elsif  PERSON = 3  then
            return "they ";
          else
            return "";
          end if;
        else
          return "";
        end if;
      end THEY;
    
      function SHALL return STRING is
      begin            --  ACTIVE only  !!!!!!!!!!!!!!!!
        if (TENSE = FUT or TENSE = FUTP )  then
          if (MOOD = IND) or (MOOD = SUB)  then
            if PERSON = 1  then
              return "shall ";
            elsif  PERSON = 2  then
              return "will ";
            elsif  PERSON = 3  then
              return "will ";
            else 
              return "";
            end if;
          elsif MOOD = IMP  then
            if PERSON = 1  then
              return "will ";
            elsif  PERSON = 2  then
              return "shall ";
            elsif  PERSON = 3  then
              return "shall ";
            else
              return "";
            end if;
          elsif MOOD = INF  then
            if TENSE = FUT  then
              return "be about to be ";
            else
              return "";
            end if;
          else
            return "";
          end if;
        else
          return "";
        end if;
      end SHALL;
  
      function HAVE return STRING is
      begin
        if TENSE in PRES..FUT  then
          return "";
        elsif TENSE = PERF  then
          if (TENSE = PERF) and (PERSON = 3) and (NUMBER = S)  then
            return "has ";
          else
            return "have ";    -- works for INF too
          end if;
        elsif TENSE = PLUP  then
          return "had ";
        elsif TENSE = FUTP   then
          return "have ";
        else
          return "";
        end if;
      end HAVE;
      
      function BEEN return STRING is
      begin
        if VOICE = PASSIVE  then
          if MOOD = IND  then
            if TENSE = PRES  then
              if (PERSON = 1) and (NUMBER = S)  then
                return "am/am being ";
              elsif (PERSON = 3) and (NUMBER = S)  then
                return "is/is being ";
              else
                return "are/are being ";
              end if;
            elsif TENSE = IMPF   then
              if (PERSON = 1 or PERSON = 3) and (NUMBER = S)  then
                return "was/was being ";
              else
                return "were/were being ";
              end if;
            elsif TENSE = FUT   then
                return "be ";
            elsif TENSE = PERF   then
              if (PERSON = 1 or PERSON = 3) and (NUMBER = S)  then
                return "been/was ";                
              else
                return "been/were ";              
              end if;
            elsif TENSE in PLUP..FUTP   then
              return "been ";
            else 
              return "";
            end if;
          elsif MOOD = SUB  then
            return "";              --????????
          elsif MOOD = INF  then
            if TENSE = PRES  then
              return "be ";
            elsif TENSE = PERF  then
              return "been ";
            else 
              return "";
            end if;
          elsif MOOD = IMP  then
            return "be ";
          else
            return "";
          end if;
        else
          return "";
        end if;
      end BEEN;
      
      function ED return STRING is
      begin
        if MOOD = IMP  then
          if VOICE = ACTIVE  then
            return " !";
          elsif VOICE = PASSIVE  then
            return "ed !";
          else
            return "";
          end if;
        end if;
        if VOICE = ACTIVE  then
          if TENSE = PRES  then
            if (PERSON = 3) and (NUMBER = S)  then
              return "s";
            else
              return "";
            end if;
          elsif TENSE = IMPF   then
            if (PERSON = 1 or PERSON = 3) and (NUMBER = S)  then
              return "ed/was _ing";
            else
              return "ed/were _ing";
            end if;
          elsif TENSE in PERF..FUTP   then
            return "ed";
          else 
            return "";
          end if;
        elsif VOICE = PASSIVE  then
          return "ed";
        else 
          return "";
        end if;
      end ED;
      
      function SUB return STRING is 
      begin
        if MOOD = SUB  then
          return "*";
        else 
          return "";
        end if;
      end SUB;


    begin   --  PUT_VERB_EXAMPLE
        if KIND = DEP    then   
          VOICE := ACTIVE;    --  Should only have allowed PASSIVE at this point
        elsif KIND = SEMIDEP    and then TENSE in PERF..FUTP   then
          VOICE := ACTIVE;    --  Should only have allowed PASSIVE at this point
        end if;
  
        TEXT_IO.PUT(OUTPUT, THEY & SUB & SHALL & HAVE & SUB & BEEN & "_" & ED);
    
    end PUT_VERB_EXAMPLE;  
    

  begin    --  PUT_EXAMPLE_LINE
    
--TEXT_IO.PUT("In EXAMPLES  "); 
--TEXT_IO.PUT("  LKM  "); BOOLEAN_IO.PUT(WORDS_MDEV(LOCK_MEANINGS));
--TEXT_IO.PUT("   /LKM  "); BOOLEAN_IO.PUT((not WORDS_MDEV(LOCK_MEANINGS))  );

    if WORDS_MODE(DO_EXAMPLES)  and then (not (CONFIGURATION = MEANINGS))   then
            
case PR.IR.QUAL.POFS is 
when N => 
  case PR.IR.QUAL.N.CS is
  when GEN =>
          TEXT_IO.PUT(OUTPUT, "_'s; of _"); 
          TEXT_IO.NEW_LINE(OUTPUT);
  when ABL =>
          TEXT_IO.NEW_LINE(OUTPUT);      --  Info too much for same line
          TEXT_IO.SET_COL(OUTPUT, 6);
          TEXT_IO.PUT(OUTPUT, 
"from _ (separ); because of _ (cause); than _ (compar); of _ (circumstance)");
          TEXT_IO.NEW_LINE(OUTPUT);
  when DAT =>
          TEXT_IO.NEW_LINE(OUTPUT);      --  Info too much for same line
          TEXT_IO.SET_COL(OUTPUT, 6);
          TEXT_IO.PUT(OUTPUT, 
      "for _ (purpose, reference); to _ (w/adjectives); to _ (double dative)");
          TEXT_IO.NEW_LINE(OUTPUT);
  when LOC =>
          TEXT_IO.PUT(OUTPUT, "at _ (place where)");
          TEXT_IO.NEW_LINE(OUTPUT);
  when others  => 
          null;
          --TEXT_IO.NEW_LINE(OUTPUT); 
  end case;

when ADJ => 
  case PR.IR.QUAL.ADJ.CO is
  when COMP  => 
          TEXT_IO.PUT(OUTPUT, "_er; more/too _");
          TEXT_IO.NEW_LINE(OUTPUT);
  when SUPER => 
          TEXT_IO.PUT(OUTPUT, "_est; most/very");
          TEXT_IO.NEW_LINE(OUTPUT);
  when others  => 
          null;
          --TEXT_IO.NEW_LINE(OUTPUT); 
  end case;

  when ADV => 
    case PR.IR.QUAL.ADV.CO is
      when COMP  => 
        TEXT_IO.PUT(OUTPUT, "more/too _(ly)");
        TEXT_IO.NEW_LINE(OUTPUT);
      when SUPER => 
        TEXT_IO.PUT(OUTPUT, "most/very _(ly)");
        TEXT_IO.NEW_LINE(OUTPUT);
      when others  => 
        null;
        --TEXT_IO.NEW_LINE(OUTPUT); 
    end case;
    
when V => 
        --TEXT_IO.NEW_LINE(OUTPUT);        --  Verb info too much for same line
        TEXT_IO.SET_COL(OUTPUT, 6);
        PUT_VERB_EXAMPLE(OUTPUT, PR, KE);
        TEXT_IO.NEW_LINE(OUTPUT);
                
when VPAR => 
  --    TEXT_IO.NEW_LINE(OUTPUT);        --  Verb info too much for same line
  case PR.IR.QUAL.VPAR.TENSE_VOICE_MOOD.TENSE is
    when PERF  => 
      TEXT_IO.PUT(OUTPUT, 
             "PERF PASSIVE PPL often used as ADJ or N (amatus => belov.ed)");
      TEXT_IO.NEW_LINE(OUTPUT);
    when PRES  => 
      TEXT_IO.PUT(OUTPUT, 
             "PRES ACTIVE PPL often used as ADJ or N (lov.ing, curl.y)");
      TEXT_IO.NEW_LINE(OUTPUT);
    when FUT   => 
      if PR.IR.QUAL.VPAR.TENSE_VOICE_MOOD.VOICE = ACTIVE  then
        TEXT_IO.PUT(OUTPUT, 
               "about to ~  FUT ACTIVE PPL often used as ADJ or N");
        TEXT_IO.NEW_LINE(OUTPUT);
      else
        TEXT_IO.PUT(OUTPUT, 
               "to(/must) be ~ed  FUT PASSIVE PPL often used as ADJ or N ");
        TEXT_IO.NEW_LINE(OUTPUT);
      end if;
    when others  => 
        null;
        --TEXT_IO.NEW_LINE(OUTPUT); 
  end case;      --  TENSE
    
  when SUPINE => 
    --TEXT_IO.NEW_LINE(OUTPUT);
    if PR.IR.QUAL.SUPINE.CS = ACC  then
        TEXT_IO.PUT(OUTPUT, 
"to ~ - expresses purpose of verb of motion; may take a direct object");
        TEXT_IO.NEW_LINE(OUTPUT);
    elsif PR.IR.QUAL.SUPINE.CS = ABL  then
        TEXT_IO.PUT(OUTPUT, 
"to ~ - after ADJ indicating aspect/respect in which something is/is done");
        TEXT_IO.NEW_LINE(OUTPUT);
    end if;   
    
when others  => 
    null;
    --TEXT_IO.NEW_LINE(OUTPUT); 
end case;        --  PART

    else
      null;
      --TEXT_IO.NEW_LINE(OUTPUT); 
    end if;

  end PUT_EXAMPLE_LINE;  
