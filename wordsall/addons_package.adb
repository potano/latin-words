with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
--with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with PREFACE;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
pragma ELABORATE(INFLECTIONS_PACKAGE);
pragma ELABORATE(DICTIONARY_PACKAGE);
package body ADDONS_PACKAGE is
  use TEXT_IO;
  use PART_OF_SPEECH_TYPE_IO;
  use TARGET_ENTRY_IO;
  use PART_ENTRY_IO;
  use INFLECTIONS_PACKAGE.INTEGER_IO;

  function EQU(C, D : CHARACTER) return BOOLEAN is
  begin
    if (D = 'u') or (D = 'v')  then
      if (C = 'u') or (C = 'v')  then
        return TRUE;
      else
        return FALSE;
      end if;
    else
      return C = D;
    end if;
  end EQU;

  function EQU(S, T : STRING) return BOOLEAN is
  begin
    if S'LENGTH /= T'LENGTH  then
      return FALSE;
    end if;

    for I in 1..S'LENGTH  loop
      if not EQU(S(S'FIRST+I-1), T(T'FIRST+I-1))  then
        return FALSE;
      end if;
    end loop;

    return TRUE;
  end EQU;


  procedure LOAD_ADDONS (FILE_NAME : in STRING) is
    use PART_OF_SPEECH_TYPE_IO;
    use TACKON_ENTRY_IO;
    use PREFIX_ENTRY_IO;
    use SUFFIX_ENTRY_IO;
    use DICT_IO;

    S : STRING(1..100);
    L, LAST, TIC, PRE, SUF, TAC, PAC : INTEGER := 0;
    ADDONS_FILE : TEXT_IO.FILE_TYPE;
    D_K : constant DICTIONARY_KIND := ADDONS;
    PART : PART_OF_SPEECH_TYPE;
    DE : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
    MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
    M : DICT_IO.POSITIVE_COUNT := 1;
    TG : TARGET_ENTRY;
    TN : TACKON_ENTRY;
    PM : PREFIX_ITEM;
    TS : STEM_TYPE;

    procedure GET_NO_COMMENT_LINE(F : in TEXT_IO.FILE_TYPE;
                                   S : out STRING; LAST : out INTEGER) is
      T : STRING(1..250) := (others => ' ');
      L : INTEGER := 0;
    begin
      LAST := 0;
      while not END_OF_FILE(F)  loop
        GET_LINE(F, T, L);
        if L >= 2  and then
           (HEAD(TRIM(T), 250)(1..2) = "--"  or
            HEAD(TRIM(T), 250)(1..2) = "  ")  then
          null;
        else
          S(1..L) := T(1..L);
          LAST := L;
          exit;
        end if;
      end loop;
    end GET_NO_COMMENT_LINE;

    procedure EXTRACT_FIX(S : in STRING;
                       XFIX : out FIX_TYPE; XC : out CHARACTER) is
      ST : constant STRING := TRIM(S);
      L : INTEGER := ST'LENGTH;
      J : INTEGER := 0;
    begin
      for I in 1..L  loop
        J := I;
        exit when ( (I < L) and then (ST(I+1) = ' ') );
      end loop;
      XFIX := HEAD(ST(1..J), MAX_FIX_SIZE);
      if J = L  then     --  there is no CONNECT CHARACTER
        XC := ' ';
        return;
      else
        for I in J+1..L  loop
          if ST(I) /= ' '  then
            XC := ST(I);
            exit;
          end if;
        end loop;
      end if;
      return;
    end EXTRACT_FIX;

  begin
    OPEN(ADDONS_FILE, IN_FILE, FILE_NAME);
    PREFACE.PUT("ADDONS");
    PREFACE.PUT(" loading ");

    if DICT_IO.IS_OPEN(DICT_FILE(D_K))  then
      DICT_IO.DELETE(DICT_FILE(D_K));
    end if;
    DICT_IO.CREATE(DICT_FILE(D_K), DICT_IO.INOUT_FILE,
          ADD_FILE_NAME_EXTENSION(DICT_FILE_NAME, DICTIONARY_KIND'IMAGE(D_K)));

    while not END_OF_FILE(ADDONS_FILE)  loop
      GET_NO_COMMENT_LINE(ADDONS_FILE, S, LAST);
      GET(S(1..LAST), PART, L);
      case PART is
        when TACKON  =>
          TS := HEAD(TRIM(S(L+1..LAST)), MAX_STEM_SIZE);

            GET_LINE(ADDONS_FILE, S, LAST);
            GET(S(1..LAST), TN, L);
            GET_LINE(ADDONS_FILE, S, LAST);
            MEAN := HEAD(S(1..LAST), MAX_MEANING_SIZE);

          if  TN.BASE.PART = PACK   and then
             (TN.BASE.PACK.DECL.WHICH = 1 or
              TN.BASE.PACK.DECL.WHICH = 2)  and then
              MEAN(1..9) = "PACKON w/"  then
            PAC := PAC + 1;
            PACKONS (PAC).PART := PART;
            PACKONS(PAC).TACK := TS;
            PACKONS(PAC).ENTR := TN;
            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
            DE.TRAN.MEAN := MEAN;
            DICT_IO.WRITE(DICT_FILE(D_K), DE);
            PACKONS (PAC).MNPC := M;
            M := M + 1;

          else
            TAC := TAC + 1;
            TACKONS (TAC).PART := PART;
            TACKONS(TAC).TACK := TS;
            TACKONS(TAC).ENTR := TN;
            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
            DE.TRAN.MEAN := MEAN;
            DICT_IO.WRITE(DICT_FILE(D_K), DE);
            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
            TACKONS (TAC).MNPC := M;
            M := M + 1;
          end if;

          NUMBER_OF_PACKONS  := PAC;
          NUMBER_OF_TACKONS  := TAC;

        when PREFIX  =>

          EXTRACT_FIX(S(L+1..LAST), PM.FIX, PM.CONNECT);
          GET_LINE(ADDONS_FILE, S, LAST);
          GET(S(1..LAST), PM.ENTR, L);
          GET_LINE(ADDONS_FILE, S, LAST);
          MEAN := HEAD(S(1..LAST), MAX_MEANING_SIZE);


          if  PM.ENTR.ROOT = PACK     then
            TIC := TIC + 1;
            TICKONS (TIC).PART := PART;
            TICKONS(TIC).FIX  := PM.FIX;
            TICKONS(TIC).CONNECT  := PM.CONNECT;
            TICKONS(TIC).ENTR := PM.ENTR;
            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
            DE.TRAN.MEAN := MEAN;
            DICT_IO.WRITE(DICT_FILE(D_K), DE);
            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
            TICKONS (TIC).MNPC := M;
            M := M + 1;

          else
            PRE := PRE + 1;
            PREFIXES(PRE).PART := PART;
            PREFIXES(PRE).FIX  := PM.FIX;
            PREFIXES(PRE).CONNECT  := PM.CONNECT;
            PREFIXES(PRE).ENTR := PM.ENTR;
            DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
            DE.TRAN.MEAN := MEAN;
            DICT_IO.WRITE(DICT_FILE(D_K), DE);
            --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
            PREFIXES(PRE).MNPC := M;
            M := M + 1;
          end if;

          NUMBER_OF_TiCKONS  := TiC;
          NUMBER_OF_PREFIXES := PRE;

        when SUFFIX  =>
        SUF := SUF + 1;
        SUFFIXES(SUF).PART := PART;
        EXTRACT_FIX(S(L+1..LAST), SUFFIXES(SUF).FIX, SUFFIXES(SUF).CONNECT);
        GET_LINE(ADDONS_FILE, S, LAST);
        GET(S(1..LAST), SUFFIXES(SUF).ENTR, L);
        GET_LINE(ADDONS_FILE, S, LAST);
        MEAN := HEAD(S(1..LAST), MAX_MEANING_SIZE);

        DICT_IO.SET_INDEX(DICT_FILE(D_K), M);
        DE.TRAN.MEAN := MEAN;
        DICT_IO.WRITE(DICT_FILE(D_K), DE);
        --DICT_IO.WRITE(DICT_FILE(D_K), MEAN);
        SUFFIXES(SUF).MNPC := M;
        M := M + 1;

        NUMBER_OF_SUFFIXES := SUF;

        when others  =>
          PUT_LINE("Bad ADDON    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
          PUT_LINE(S(1..LAST));
          raise TEXT_IO.DATA_ERROR;
      end case;

    end loop;

    PREFACE.PUT(TAC, 1); PREFACE.PUT("+");
    PREFACE.PUT(PAC, 2); PREFACE.PUT(" TACKONS ");
    PREFACE.PUT(TIC, 1); PREFACE.PUT("+");
    PREFACE.PUT(PRE, 3); PREFACE.PUT(" PREFIXES ");
    PREFACE.PUT(SUF, 3); PREFACE.PUT(" SUFFIXES ");

    PREFACE.SET_COL(60); PREFACE.PUT_LINE("--  Loaded correctly");
    CLOSE(ADDONS_FILE);

  exception
    when TEXT_IO.NAME_ERROR  =>
      PREFACE.PUT_LINE("No ADDONS file ");
      null;
    when TEXT_IO.DATA_ERROR  =>
      PREFACE.PUT_LINE(S(1..LAST));
      PREFACE.PUT_LINE("No further ADDONS read ");
      CLOSE(ADDONS_FILE);
    when others      =>
      PREFACE.PUT_LINE("Exception in LOAD_ADDONS");
      PREFACE.PUT_LINE(S(1..LAST));
  end LOAD_ADDONS;

  function SUBTRACT_TACKON(W : STRING; X : TACKON_ITEM) return STRING is
    WD : constant STRING := TRIM(W);
    L  : constant INTEGER := WD'LENGTH;
    XF : constant STRING := TRIM(X.TACK);
    Z  : constant INTEGER := XF'LENGTH;
  begin
--PUT_LINE("In SUB TACKON " & INTEGER'IMAGE(L) & INTEGER'IMAGE(Z));
    if L > Z  and then
       --WD(L-Z+1..L) = XF(1..Z)  then
       EQU(WD(L-Z+1..L),  XF(1..Z)) then
--PUT("In SUBTRACT_TACKON we got a hit   "); PUT_LINE(X.TACK);
      return WD(1..L-Z);
    else
--PUT("In SUBTRACT_TACKON    NO    hit   "); PUT_LINE(X.TACK);
      return W;
    end if;
  end SUBTRACT_TACKON;

  function SUBTRACT_PREFIX(W : STRING; X : PREFIX_ITEM) return STEM_TYPE is
    WD : constant STRING := TRIM(W);
    XF : constant STRING := TRIM(X.FIX);
    Z  : constant INTEGER := XF'LENGTH;
    ST : STEM_TYPE := HEAD(WD, MAX_STEM_SIZE);
  begin
    if X /= NULL_PREFIX_ITEM and then
       WD'LENGTH > Z  and then
       --WD(1..Z) = XF(1..Z)  and then
       EQU(WD(1..Z),  XF(1..Z)) and then
    ( (X.CONNECT = ' ') or (WD(Z+1) = X.CONNECT) )  then
      ST(1..WD'LENGTH-Z) := WD(Z+1..WD'LAST);
      ST(WD'LENGTH-Z+1..MAX_STEM_SIZE) :=
          NULL_STEM_TYPE(WD'LENGTH-Z+1..MAX_STEM_SIZE);
    end if;
--PUT_LINE("SUBTRACT_PREFIX  " & X.FIX & " FROM " & WD & "  returns " & ST);
    return ST;
  end SUBTRACT_PREFIX;

  function SUBTRACT_SUFFIX(W : STRING; X : SUFFIX_ITEM) return STEM_TYPE is
    WD : constant STRING := TRIM(W);
    L  : constant INTEGER := WD'LENGTH;
    XF : constant STRING := TRIM(X.FIX);
    Z  : constant INTEGER := XF'LENGTH;
    ST : STEM_TYPE := HEAD(WD, MAX_STEM_SIZE);
  begin
--PUT_LINE("In SUBTRACT_SUFFIX  Z = " & INTEGER'IMAGE(Z) & 
--"  CONNECT >" & X.CONNECT & '<');
    if X /= NULL_SUFFIX_ITEM and then
       WD'LENGTH > Z  and then
       --WD(L-Z+1..L) = XF(1..Z)  and then
       EQU(WD(L-Z+1..L),  XF(1..Z))  and then
    ( (X.CONNECT = ' ') or (WD(L-Z) = X.CONNECT) )  then
--PUT_LINE("In SUBTRACT_SUFFIX we got a hit");
      ST(1..WD'LENGTH-Z) := WD(1..WD'LENGTH-Z);
      ST(WD'LENGTH-Z+1..MAX_STEM_SIZE) :=
          NULL_STEM_TYPE(WD'LENGTH-Z+1..MAX_STEM_SIZE);
    end if;
--PUT_LINE("SUBTRACT_SUFFIX  " & X.FIX & " FROM " & WD & "  returns " & ST);
    return ST;
  end SUBTRACT_SUFFIX;

  function ADD_PREFIX(STEM : STEM_TYPE;
                    PREFIX : PREFIX_ITEM) return STEM_TYPE is
    FPX : constant STRING := TRIM(PREFIX.FIX) & STEM;
  begin
    return HEAD(FPX, MAX_STEM_SIZE);
  end ADD_PREFIX;

  function ADD_SUFFIX(STEM : STEM_TYPE;
                    SUFFIX : SUFFIX_ITEM) return STEM_TYPE is
    FPX : constant STRING := TRIM(STEM) & SUFFIX.FIX;
  begin
    return HEAD(FPX, MAX_STEM_SIZE);
  end ADD_SUFFIX;


--  package body TARGET_ENTRY_IO is separate;

--  package body TACKON_ENTRY_IO is separate;

--  package body TACKON_LINE_IO is separate;

--  package body PREFIX_ENTRY_IO is separate;

--  package body PREFIX_LINE_IO is separate;

--  package body SUFFIX_ENTRY_IO is separate;

--  package body SUFFIX_LINE_IO is separate;


  package body TARGET_ENTRY_IO is
  use PART_OF_SPEECH_TYPE_IO;
  use NOUN_ENTRY_IO;
  use PRONOUN_ENTRY_IO;
  use PROPACK_ENTRY_IO;
  use ADJECTIVE_ENTRY_IO;
  use NUMERAL_ENTRY_IO;
  use ADVERB_ENTRY_IO;
  use VERB_ENTRY_IO;
  SPACER : CHARACTER := ' ';

  NOUN  : NOUN_ENTRY;
  PRONOUN : PRONOUN_ENTRY;
  PROPACK : PROPACK_ENTRY;
  ADJECTIVE : ADJECTIVE_ENTRY;
  NUMERAL : NUMERAL_ENTRY;
  ADVERB : ADVERB_ENTRY;
  VERB : VERB_ENTRY;


  P : TARGET_ENTRY;


  procedure GET(F : in FILE_TYPE; P : out TARGET_ENTRY) is
    PS : TARGET_PART_TYPE := X;
  begin
    GET(F, PS);
    GET(F, SPACER);
    case PS is
      when N =>
        GET(F, NOUN);
        P := (N, NOUN);
      when PRON =>
        GET(F, PRONOUN);
        P := (PRON, PRONOUN);
      when PACK =>
        GET(F, PROPACK);
        P := (PACK, PROPACK);
      when ADJ =>
        GET(F, ADJECTIVE);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(F, NUMERAL);
        P := (NUM, NUMERAL);
      when ADV =>
        GET(F, ADVERB);
        P := (ADV, ADVERB);
      when V =>
        GET(F, VERB);
        P := (V, VERB);
      when X =>
        P := (PART => X);
    end case;
    return;
  end GET;

  procedure GET(P : out TARGET_ENTRY) is
    PS : TARGET_PART_TYPE := X;
  begin
    GET(PS);
    GET(SPACER);
    case PS is
      when N =>
        GET(NOUN);
        P := (N, NOUN);
      when PRON =>
        GET(PRONOUN);
        P := (PRON, PRONOUN);
      when PACK =>
        GET(PROPACK);
        P := (PACK, PROPACK);
      when ADJ =>
        GET(ADJECTIVE);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(NUMERAL);
        P := (NUM, NUMERAL);
      when ADV =>
        GET(ADVERB);
        P := (ADV, ADVERB);
      when V =>
        GET(VERB);
        P := (V, VERB);
      when X =>
        P := (PART => X);
    end case;
    return;
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in TARGET_ENTRY) is
    C : POSITIVE := POSITIVE(COL(F));
  begin
    PUT(F, P.PART);
    PUT(F, ' ');
    case P.PART is
      when N =>
        PUT(F, P.N);
      when PRON =>
        PUT(F, P.PRON);
      when PACK =>
        PUT(F, P.PACK);
      when ADJ =>
        PUT(F, P.ADJ);
      when NUM =>
        PUT(F, P.NUM);
      when ADV =>
        PUT(F, P.ADV);
      when V =>
        PUT(F, P.V);
      when others =>
        null;
    end case;
    PUT(F, STRING'((INTEGER(COL(F))..TARGET_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
  return;
  end PUT;


  procedure PUT(P : in TARGET_ENTRY) is
    C : POSITIVE := POSITIVE(COL);
  begin
    PUT(P.PART);
    PUT(' ');
    case P.PART is
      when N =>
        PUT(P.N);
      when PRON =>
        PUT(P.PRON);
      when PACK =>
        PUT(P.PACK);
      when ADJ =>
        PUT(P.ADJ);
      when NUM =>
        PUT(P.NUM);
      when ADV =>
        PUT(P.ADV);
      when V =>
        PUT(P.V);
      when others =>
        null;
    end case;
    PUT(STRING'((INTEGER(COL)..TARGET_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
    return;
  end PUT;

  procedure GET(S : in STRING; P : out TARGET_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
    PS : TARGET_PART_TYPE := X;
  begin
    GET(S, PS, L);
    L := L + 1;
    case PS is
      when N =>
        GET(S(L+1..S'LAST), NOUN, LAST);
        P := (N, NOUN);
      when PRON =>
        GET(S(L+1..S'LAST), PRONOUN, LAST);
        P := (PRON, PRONOUN);
      when PACK =>
        GET(S(L+1..S'LAST), PROPACK, LAST);
        P := (PACK, PROPACK);
      when ADJ =>
        GET(S(L+1..S'LAST), ADJECTIVE, LAST);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(S(L+1..S'LAST), NUMERAL, LAST);
        P := (NUM, NUMERAL);
      when ADV =>
        GET(S(L+1..S'LAST), ADVERB, LAST);
        P := (ADV, ADVERB);
      when V =>
        GET(S(L+1..S'LAST), VERB, LAST);
        P := (V, VERB);
      when X =>
        P := (PART => X);
    end case;
    return;
  end GET;


  procedure PUT(S : out STRING; P : in TARGET_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.PART);
    L := M + 1;
    S(L) :=  ' ';
    case P.PART is
      when N =>
        M := L + NOUN_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.N);
      when PRON =>
        M := L + PRONOUN_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PRON);
      when PACK =>
        M := L + PROPACK_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PACK);
      when ADJ =>
        M := L + ADJECTIVE_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.ADJ);
      when NUM =>
        M := L + NUMERAL_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.NUM);
      when ADV =>
        M := L + ADVERB_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.ADV);
      when V =>
        M := L + VERB_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.V);
      when others =>
        null;
    end case;
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end TARGET_ENTRY_IO;


package body TACKON_ENTRY_IO is
  SPACER : CHARACTER := ' ';

  procedure GET(F : in FILE_TYPE; I : out TACKON_ENTRY) is
  begin
    GET(F, I.BASE);
  end GET;

  procedure GET(I : out TACKON_ENTRY) is
  begin
    GET(I.BASE);
  end GET;

  procedure PUT(F : in FILE_TYPE; I : in TACKON_ENTRY) is
  begin
    PUT(F, I.BASE);
  end PUT;

  procedure PUT(I : in TACKON_ENTRY) is
  begin
    PUT(I.BASE);
  end PUT;

  procedure GET(S : in STRING; I : out TACKON_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + TARGET_ENTRY_IO.DEFAULT_WIDTH;
    GET(S(L+1..S'LAST), I.BASE, LAST);
  end GET;

  procedure PUT(S : out STRING; I : in TACKON_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + TARGET_ENTRY_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), I.BASE);
    S(S'FIRST..S'LAST) := (others => ' ');
  end PUT;


end TACKON_ENTRY_IO;


  package body PREFIX_ENTRY_IO is
    use PART_OF_SPEECH_TYPE_IO;
    use TEXT_IO;
    SPACER : CHARACTER := ' ';

    PE : PREFIX_ENTRY;

    procedure GET(F : in FILE_TYPE; P : out PREFIX_ENTRY) is
    begin
      GET(F, P.ROOT);
      GET(F, SPACER);
      GET(F, P.TARGET);
     end GET;


    procedure GET(P : out PREFIX_ENTRY) is
    begin
      GET(P.ROOT);
      GET(SPACER);
      GET(P.TARGET);
     end GET;

    procedure PUT(F : in FILE_TYPE; P : in PREFIX_ENTRY) is
    begin
      PUT(F, P.ROOT);
      PUT(F, ' ');
      PUT(F, P.TARGET);
     end PUT;

    procedure PUT(P : in PREFIX_ENTRY) is
    begin
      PUT(P.ROOT);
      PUT(' ');
      PUT(P.TARGET);
     end PUT;

    procedure GET(S : in STRING; P : out PREFIX_ENTRY; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.ROOT, L);
      L := L + 1;
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.TARGET, LAST);
    end GET;


    procedure PUT(S : out STRING; P : in PREFIX_ENTRY) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.ROOT);
      L := M + 1;
      S(L) :=  ' ';
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.TARGET);
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end PREFIX_ENTRY_IO;



  package body SUFFIX_ENTRY_IO is
    use PART_OF_SPEECH_TYPE_IO;
    use TARGET_ENTRY_IO;
    use TEXT_IO;
    SPACER : CHARACTER := ' ';

    PE : SUFFIX_ENTRY;

    procedure GET(F : in FILE_TYPE; P : out SUFFIX_ENTRY) is
    begin
      GET(F, P.ROOT);
      GET(F, SPACER);
      GET(F, P.ROOT_KEY);
      GET(F, SPACER);
      GET(F, P.TARGET);
      GET(F, SPACER);
      GET(F, P.TARGET_KEY);
     end GET;


    procedure GET(P : out SUFFIX_ENTRY) is
    begin
      GET(P.ROOT);
      GET(SPACER);
      GET(P.ROOT_KEY);
      GET(SPACER);
      GET(P.TARGET);
      GET(SPACER);
      GET(P.TARGET_KEY);
     end GET;

    procedure PUT(F : in FILE_TYPE; P : in SUFFIX_ENTRY) is
    begin
      PUT(F, P.ROOT);
      PUT(F, ' ');
      PUT(F, P.ROOT_KEY, 2);
      PUT(F, ' ');
      PUT(F, P.TARGET);
      PUT(F, ' ');
      PUT(F, P.TARGET_KEY, 2);
     end PUT;

    procedure PUT(P : in SUFFIX_ENTRY) is
    begin
      PUT(P.ROOT);
      PUT(' ');
      PUT(P.ROOT_KEY, 2);
      PUT(' ');
      PUT(P.TARGET);
      PUT(' ');
      PUT(P.TARGET_KEY, 2);
     end PUT;

    procedure GET(S : in STRING; P : out SUFFIX_ENTRY; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.ROOT, L);
      L := L + 1;
      M := L + 2;
      GET(S(L+1..S'LAST), P.ROOT_KEY, L);
      L := L + 1;
      M := L + TARGET_ENTRY_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), P.TARGET, L);
      L := L + 1;
      M := L + 2;
      GET(S(L+1..S'LAST), P.TARGET_KEY, LAST);
    end GET;


    procedure PUT(S : out STRING; P : in SUFFIX_ENTRY) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.ROOT);
      L := M + 1;
      S(L) :=  ' ';
      M := L + 2;
      PUT(S(L+1..M), P.ROOT_KEY);
      L := M + 1;
      S(L) :=  ' ';
      M := L + TARGET_ENTRY_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), P.TARGET);
      L := M + 1;
      S(L) :=  ' ';
      M := L + 2;
      PUT(S(L+1..M), P.TARGET_KEY);
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end SUFFIX_ENTRY_IO;



 begin    --  Initiate body of ADDONS_PACKAGE
--TEXT_IO.PUT_LINE("Initializing ADDONS_PACKAGE");

  PREFIX_ENTRY_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                   PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
  TARGET_ENTRY_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                   VERB_ENTRY_IO.DEFAULT_WIDTH; --  Largest

  SUFFIX_ENTRY_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                   2 + 1 +
                                   TARGET_ENTRY_IO.DEFAULT_WIDTH + 1 +
                                   2;
  TACKON_ENTRY_IO.DEFAULT_WIDTH := TARGET_ENTRY_IO.DEFAULT_WIDTH;

end ADDONS_PACKAGE;