with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
pragma ELABORATE(INFLECTIONS_PACKAGE);
package body DICTIONARY_PACKAGE is
  use INFLECTIONS_PACKAGE.INTEGER_IO;
  use TEXT_IO;


  function NUMBER_OF_STEMS(P : PART_OF_SPEECH_TYPE) return INTEGER is
  begin
    case P is
      when N       => return 2;
      when PRON    => return 2;
      when PACK    => return 2;
      when ADJ     => return 4;
      when NUM     => return 4;
      when ADV     => return 3;
      when V       => return 4;
      when VPAR    => return 0;
      when SUPINE  => return 0;
      when PREP    => return 1;
      when CONJ    => return 1;
      when INTERJ  => return 1;
      when others  => return 0;
    end case;
  end NUMBER_OF_STEMS;



package body AAMNPC_RECORD_IO is
    use TEXT_IO;
    use AGE_TYPE_IO;
    use AREA_TYPE_IO;
    use GEO_TYPE_IO;
    use FREQUENCY_TYPE_IO;
    use SOURCE_TYPE_IO;
    use MNPC_IO;
    SPACER : CHARACTER := ' ';

    procedure GET(F : in TEXT_IO.FILE_TYPE; AA : out AAMNPC_RECORD) is
    begin
      GET(F, AA.AGE);
      GET(F, SPACER);
      GET(F, AA.AREA);
      GET(F, SPACER);
      GET(F, AA.GEO);
      GET(F, SPACER);
      GET(F, AA.FREQ);
      GET(F, SPACER);
      GET(F, AA.SOURCE);
      GET(F, SPACER);
      GET(F, AA.MNPC);
    end GET;

    procedure GET(AA : out AAMNPC_RECORD) is
    begin
      GET(AA.AGE);
      GET(SPACER);
      GET(AA.AREA);
      GET(SPACER);
      GET(AA.GEO);
      GET(SPACER);
      GET(AA.FREQ);
      GET(SPACER);
      GET(AA.SOURCE);
      GET(SPACER);
      GET(AA.MNPC);
    end GET;

    procedure PUT(F : in TEXT_IO.FILE_TYPE; AA : in AAMNPC_RECORD) is
    begin
      PUT(F, AA.AGE);
      PUT(F, ' ');
      PUT(F, AA.AREA);
      PUT(F, ' ');
      PUT(F, AA.GEO);
      PUT(F, ' ');
      PUT(F, AA.FREQ);
      PUT(F, ' ');
      PUT(F, AA.SOURCE);
      PUT(F, ' ');
      PUT(F, AA.MNPC);
    end PUT;

    procedure PUT(AA : in AAMNPC_RECORD) is
    begin
      PUT(AA.AGE);
      TEXT_IO.PUT(' ');
      PUT(AA.AREA);
      TEXT_IO.PUT(' ');
      PUT(AA.GEO);
      TEXT_IO.PUT(' ');
      PUT(AA.FREQ);
      TEXT_IO.PUT(' ');
      PUT(AA.SOURCE);
      TEXT_IO.PUT(' ');
      MNPC_IO.PUT(AA.MNPC);
    end PUT;

    procedure GET(S : in STRING; AA : out AAMNPC_RECORD; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + AGE_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), AA.AGE, L);
      L := M + 1;
      M := L + AREA_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), AA.AREA, L);
      L := M + 1;
      M := L + GEO_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), AA.GEO, L);
      L := M + 1;
      M := L + FREQUENCY_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), AA.FREQ, L);
      L := M + 1;
      M := L + SOURCE_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), AA.SOURCE, L);
      L := M + 1;
      GET(S(L+1..S'LAST), AA.MNPC, L);
      LAST := L;
    end GET;

    procedure PUT(S : out STRING; AA : in AAMNPC_RECORD) is
      L : INTEGER := 0;
      M : INTEGER := 0;
    begin
      M := L + AGE_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), AA.AGE);
      L := M + 1;
      S(L) :=  ' ';
      M := L + AREA_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), AA.AREA);
      L := M + 1;
      S(L) :=  ' ';
      M := L + GEO_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), AA.GEO);
      L := M + 1;
      S(L) :=  ' ';
      M := L + FREQUENCY_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), AA.FREQ);
      L := M + 1;
      S(L) :=  ' ';
      M := L + SOURCE_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), AA.SOURCE);
      L := M + 1;
      S(L) :=  ' ';
      --M := L + MAX_MEANING_SIZE;
      M := L + 6;
      PUT(S(L+1..M), AA.MNPC);
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end AAMNPC_RECORD_IO;



  package body PARSE_RECORD_IO is
    use TEXT_IO;
    use INFLECTION_RECORD_IO;
    use DICTIONARY_KIND_IO;
    use AAMNPC_RECORD_IO;
    SPACER : CHARACTER := ' ';

    procedure GET(F : in TEXT_IO.FILE_TYPE; PR: out PARSE_RECORD) is
    begin
      GET(F, PR.STEM);
      GET(F, SPACER);
      GET(F, PR.IR);
      GET(F, SPACER);
      GET(F, PR.D_K);
      GET(F, SPACER);
      GET(F, PR.AAMNPC);
    end GET;

    procedure GET(PR : out PARSE_RECORD) is
    begin
      GET(PR.STEM);
      GET(SPACER);
      GET(PR.IR);
      GET(SPACER);
      GET(PR.D_K);
      GET(SPACER);
      GET(PR.AAMNPC);
    end GET;

    procedure PUT(F : in TEXT_IO.FILE_TYPE; PR : in PARSE_RECORD) is
    begin
      PUT(F, PR.STEM);
      PUT(F, ' ');
      PUT(F, PR.IR);
      PUT(F, ' ');
      PUT(F, PR.D_K);
      PUT(F, ' ');
      PUT(F, PR.AAMNPC);
    end PUT;

    procedure PUT(PR : in PARSE_RECORD) is
    begin
      TEXT_IO.PUT(PR.STEM);
      TEXT_IO.PUT(' ');
      INFLECTION_RECORD_IO.PUT(PR.IR);
      TEXT_IO.PUT(' ');
      DICTIONARY_KIND_IO.PUT(PR.D_K);
      TEXT_IO.PUT(' ');
      AAMNPC_RECORD_IO.PUT(PR.AAMNPC);
    end PUT;

    procedure GET(S : in STRING; PR : out PARSE_RECORD; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + MAX_STEM_SIZE;
      PR.STEM := S(L+1..M);
      L := M + 1;
      M := L + INFLECTION_RECORD_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), PR.IR, L);
      L := M + 1;
      M := L + DICTIONARY_KIND_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), PR.D_K, L);
      L := M + 1;
      M := L + MAX_MEANING_SIZE;
      GET(S(L+1..S'LAST), PR.AAMNPC, L);
      LAST := M;
    end GET;

    procedure PUT(S : out STRING; PR : in PARSE_RECORD) is
      L : INTEGER := 0;
      M : INTEGER := 0;
    begin
      M := L + MAX_STEM_SIZE;
      S(L+1..M) := PR.STEM;
      L := M + 1;
      S(L) :=  ' ';
      M := L + INFLECTION_RECORD_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), PR.IR);
      L := M + 1;
      S(L) :=  ' ';
      M := L + DICTIONARY_KIND_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), PR.D_K);
      L := M + 1;
      S(L) :=  ' ';
      M := L + MAX_MEANING_SIZE;
      PUT(S(L+1..M), PR.AAMNPC);
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

  end PARSE_RECORD_IO;

package body NOUN_ENTRY_IO is
  use DECN_RECORD_IO;
  use GENDER_TYPE_IO;
  use NOUN_KIND_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; N : out NOUN_ENTRY) is
  begin
    GET(F, N.DECL);
    GET(F, SPACER);
    GET(F, N.GENDER);
    GET(F, SPACER);
    GET(F, N.KIND);
  end GET;

  procedure GET(N : out NOUN_ENTRY) is
  begin
    GET(N.DECL);
    GET(SPACER);
    GET(N.GENDER);
    GET(SPACER);
    GET(N.KIND);
  end GET;

  procedure PUT(F : in FILE_TYPE; N : in NOUN_ENTRY) is
  begin
    PUT(F, N.DECL);
    PUT(F, ' ');
    PUT(F, N.GENDER);
    PUT(F, ' ');
    PUT(F, N.KIND);
  end PUT;

  procedure PUT(N : in NOUN_ENTRY) is
  begin
    PUT(N.DECL);
    PUT(' ');
    PUT(N.GENDER);
    PUT(' ');
    PUT(N.KIND);
  end PUT;

  procedure GET(S : in STRING; N : out NOUN_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), N.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), N.GENDER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), N.KIND, LAST);
  end GET;

  procedure PUT(S : out STRING; N : in NOUN_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), N.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + GENDER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), N.GENDER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), N.KIND);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end NOUN_ENTRY_IO;


package body PRONOUN_ENTRY_IO is
  use DECN_RECORD_IO;
  use PRONOUN_KIND_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; P : out PRONOUN_ENTRY) is
  begin
    GET(F, P.DECL);
    GET(F, SPACER);
    GET(F, P.KIND);
  end GET;

  procedure GET(P : out PRONOUN_ENTRY) is
  begin
    GET(P.DECL);
    GET(SPACER);
    GET(P.KIND);
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in PRONOUN_ENTRY) is
  begin
    PUT(F, P.DECL);
    PUT(F, ' ');
    PUT(F, P.KIND);
  end PUT;

  procedure PUT(P : in PRONOUN_ENTRY) is
  begin
    PUT(P.DECL);
    PUT(' ');
    PUT(P.KIND);
  end PUT;

  procedure GET(S : in STRING; P : out PRONOUN_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), P.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.KIND, LAST);
  end GET;

  procedure PUT(S : out STRING; P : in PRONOUN_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.KIND);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end PRONOUN_ENTRY_IO;


package body PROPACK_ENTRY_IO is
  use DECN_RECORD_IO;
  use PRONOUN_KIND_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; P : out PROPACK_ENTRY) is
  begin
    GET(F, P.DECL);
    GET(F, SPACER);
    GET(F, P.KIND);
  end GET;

  procedure GET(P : out PROPACK_ENTRY) is
  begin
    GET(P.DECL);
    GET(SPACER);
    GET(P.KIND);
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in PROPACK_ENTRY) is
  begin
    PUT(F, P.DECL);
    PUT(F, ' ');
    PUT(F, P.KIND);
  end PUT;

  procedure PUT(P : in PROPACK_ENTRY) is
  begin
    PUT(P.DECL);
    PUT(' ');
    PUT(P.KIND);
  end PUT;

  procedure GET(S : in STRING; P : out PROPACK_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), P.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.KIND, LAST);
  end GET;

  procedure PUT(S : out STRING; P : in PROPACK_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.KIND);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end PROPACK_ENTRY_IO;


package body ADJECTIVE_ENTRY_IO is
  use DECN_RECORD_IO;
  use GENDER_TYPE_IO;
  use CASE_TYPE_IO;
  use NUMBER_TYPE_IO;
  use COMPARISON_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; A : out ADJECTIVE_ENTRY) is
  begin
    GET(F, A.DECL);
    GET(F, SPACER);
    GET(F, A.CO);
  end GET;

  procedure GET(A : out ADJECTIVE_ENTRY) is
  begin
    GET(A.DECL);
    GET(SPACER);
    GET(A.CO);
  end GET;

  procedure PUT(F : in FILE_TYPE; A : in ADJECTIVE_ENTRY) is
  begin
    PUT(F, A.DECL);
    PUT(F, ' ');
    PUT(F, A.CO);
  end PUT;

  procedure PUT(A : in ADJECTIVE_ENTRY) is
  begin
    PUT(A.DECL);
    PUT(' ');
    PUT(A.CO);
  end PUT;

  procedure GET(S : in STRING; A : out ADJECTIVE_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), A.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), A.CO, LAST);
  end GET;

  procedure PUT(S : out STRING; A : in ADJECTIVE_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + COMPARISON_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.CO);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end ADJECTIVE_ENTRY_IO;


package body ADVERB_ENTRY_IO is
  use COMPARISON_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; A : out ADVERB_ENTRY) is
  begin
    GET(F, A.CO);
  end GET;

  procedure GET(A : out ADVERB_ENTRY) is
  begin
    GET(A.CO);
  end GET;

  procedure PUT(F : in FILE_TYPE; A : in ADVERB_ENTRY) is
  begin
    PUT(F, A.CO);
  end PUT;

  procedure PUT(A : in ADVERB_ENTRY) is
  begin
    PUT(A.CO);
  end PUT;

  procedure GET(S : in STRING; A : out ADVERB_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), A.CO, LAST);
  end GET;

  procedure PUT(S : out STRING; A : in ADVERB_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + COMPARISON_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.CO);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end ADVERB_ENTRY_IO;


package body VERB_ENTRY_IO is
  use DECN_RECORD_IO;
  use VERB_KIND_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; V : out VERB_ENTRY) is
  begin
    GET(F, V.CON);
    GET(F, SPACER);
    GET(F, V.KIND);
  end GET;

  procedure GET(V : out VERB_ENTRY) is
  begin
    GET(V.CON);
    GET(SPACER);
    GET(V.KIND);
  end GET;

  procedure PUT(F : in FILE_TYPE; V : in VERB_ENTRY) is
  begin
    PUT(F, V.CON);
    PUT(F, ' ');
    PUT(F, V.KIND);
  end PUT;

  procedure PUT(V : in VERB_ENTRY) is
  begin
    PUT(V.CON);
    PUT(' ');
    PUT(V.KIND);
  end PUT;
  procedure GET(S : in STRING; V : out VERB_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), V.CON, L);
    L := L + 1;
    GET(S(L+1..S'LAST), V.KIND, LAST);
  end GET;

  procedure PUT(S : out STRING; V : in VERB_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), V.CON);
    L := M + 1;
    S(L) :=  ' ';
    M := L + VERB_KIND_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), V.KIND);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end VERB_ENTRY_IO;


package body PREPOSITION_ENTRY_IO is
  use CASE_TYPE_IO;
  SPACER : CHARACTER := ' ';

  procedure GET(F : in FILE_TYPE; P : out PREPOSITION_ENTRY) is
  begin
    GET(F, P.OBJ);
  end GET;

  procedure GET(P : out PREPOSITION_ENTRY) is
  begin
    GET(P.OBJ);
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in PREPOSITION_ENTRY) is
  begin
    PUT(F, P.OBJ);
  end PUT;

  procedure PUT(P : in PREPOSITION_ENTRY) is
  begin
    PUT(P.OBJ);
  end PUT;

  procedure GET(S : in STRING; P : out PREPOSITION_ENTRY; LAST : out INTEGER) is
  begin
    GET(S, P.OBJ, LAST);
  end GET;

  procedure PUT(S : out STRING; P : in PREPOSITION_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.OBJ);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end PREPOSITION_ENTRY_IO;


package body CONJUNCTION_ENTRY_IO is
  NULL_CONJUNCTION_ENTRY : CONJUNCTION_ENTRY;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; C : out CONJUNCTION_ENTRY) is
  begin
    C := NULL_CONJUNCTION_ENTRY;
  end GET;

  procedure GET(C : out CONJUNCTION_ENTRY) is
  begin
    C := NULL_CONJUNCTION_ENTRY;
  end GET;

  procedure PUT(F : in FILE_TYPE; C : in CONJUNCTION_ENTRY) is
  begin
    null;
  end PUT;

  procedure PUT(C : in CONJUNCTION_ENTRY) is
  begin
    null;
  end PUT;

  procedure GET(S : in STRING; C : out CONJUNCTION_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    C := NULL_CONJUNCTION_ENTRY;
    LAST := L;
  end GET;

  procedure PUT(S : out STRING; C : in CONJUNCTION_ENTRY) is
  begin
    S(S'FIRST..S'LAST) := (others => ' ');
  end PUT;


end CONJUNCTION_ENTRY_IO;


package body INTERJECTION_ENTRY_IO is
  NULL_INTERJECTION_ENTRY : INTERJECTION_ENTRY;
  SPACER : CHARACTER := ' ';

 procedure GET(F : in FILE_TYPE; I : out INTERJECTION_ENTRY) is
  begin
    I := NULL_INTERJECTION_ENTRY;
  end GET;

  procedure GET(I : out INTERJECTION_ENTRY) is
  begin
    I := NULL_INTERJECTION_ENTRY;
  end GET;

  procedure PUT(F : in FILE_TYPE; I : in INTERJECTION_ENTRY) is
  begin
    null;
  end PUT;

  procedure PUT(I : in INTERJECTION_ENTRY) is
  begin
    null;
  end PUT;

  procedure GET(S : in STRING; I : out INTERJECTION_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    I := NULL_INTERJECTION_ENTRY;
    LAST := L;
  end GET;

  procedure PUT(S : out STRING; I : in INTERJECTION_ENTRY) is
  begin
    S(S'FIRST..S'LAST) := (others => ' ');
  end PUT;


end INTERJECTION_ENTRY_IO;


package body NUMERAL_ENTRY_IO is
  use DECN_RECORD_IO;
  use NUMERAL_KIND_TYPE_IO;
  SPACER : CHARACTER := ' ';

  NUM_OUT_SIZE : constant := 5;    --  Set in spec  !!!!!!!!!!!!!!!!!!!!!!!!!


  procedure GET(F : in FILE_TYPE; NUM : out NUMERAL_ENTRY) is
  begin
    GET(F, NUM.DECL);
    GET(F, SPACER);
    GET(F, NUM.KIND);
    GET(F, SPACER);
    GET(F, NUM.VALUE);
  end GET;

  procedure GET(NUM : out NUMERAL_ENTRY) is
  begin
    GET(NUM.DECL);
    GET(SPACER);
    GET(NUM.KIND);
    GET(SPACER);
    GET(NUM.VALUE);
  end GET;

  procedure PUT(F : in FILE_TYPE; NUM : in NUMERAL_ENTRY) is
  begin
    PUT(F, NUM.DECL);
    PUT(F, ' ');
    PUT(F, NUM.KIND);
    PUT(F, ' ');
    PUT(F, NUM.VALUE, NUM_OUT_SIZE);
  end PUT;

  procedure PUT(NUM : in NUMERAL_ENTRY) is
  begin
    PUT(NUM.DECL);
    PUT(' ');
    PUT(NUM.KIND);
    PUT(' ');
    PUT(NUM.VALUE, NUM_OUT_SIZE);
  end PUT;

  procedure GET(S : in STRING; NUM : out NUMERAL_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), NUM.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), NUM.KIND, L);
    L := L + 1;
    GET(S(L+1..S'LAST), NUM.VALUE, LAST);
  end GET;

  procedure PUT(S : out STRING; NUM : in NUMERAL_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), NUM.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMERAL_KIND_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), NUM.KIND);
    L := M + 1;
    S(L) :=  ' ';
    --M := L + NUMERAL_VALUE_TYPE_IO.DEFAULT_WIDTH;
    M := L + NUM_OUT_SIZE;
    PUT(S(L+1..M), NUM.VALUE);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end NUMERAL_ENTRY_IO;


package body PART_ENTRY_IO is
  use PART_OF_SPEECH_TYPE_IO;
  use NOUN_ENTRY_IO;
  use PRONOUN_ENTRY_IO;
  use PROPACK_ENTRY_IO;
  use ADJECTIVE_ENTRY_IO;
  use NUMERAL_ENTRY_IO;
  use ADVERB_ENTRY_IO;
  use VERB_ENTRY_IO;
  use PREPOSITION_ENTRY_IO;
  use CONJUNCTION_ENTRY_IO;
  use INTERJECTION_ENTRY_IO;
  SPACER : CHARACTER := ' ';


  NOUN : NOUN_ENTRY;
  PRONOUN : PRONOUN_ENTRY;
  PROPACK : PROPACK_ENTRY;
  ADJECTIVE : ADJECTIVE_ENTRY;
  NUMERAL : NUMERAL_ENTRY;
  ADVERB : ADVERB_ENTRY;
  VERB : VERB_ENTRY;
  PREPOSITION : PREPOSITION_ENTRY;
  CONJUNCTION : CONJUNCTION_ENTRY;
  INTERJECTION : INTERJECTION_ENTRY;

  PR : PART_ENTRY;


  procedure GET(F : in FILE_TYPE; P : out PART_ENTRY) is
    PS : PART_OF_SPEECH_TYPE := X;
    C : POSITIVE_COUNT := COL(F);
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
      when VPAR =>
        null;                --  No VAPR entry
      when SUPINE =>
        null;                --  No SUPINE entry
      when PREP =>
        GET(F, PREPOSITION);
        P := (PREP, PREPOSITION);
      when CONJ =>
        GET(F, CONJUNCTION);
        P := (CONJ, CONJUNCTION);
      when INTERJ =>
        GET(F, INTERJECTION);
        P := (INTERJ, INTERJECTION);
      when PREFIX =>
        P := (PART => PREFIX);
      when SUFFIX =>
        P := (PART => SUFFIX);
      when TACKON =>
        P := (PART => TACKON);
      when X =>
        P := (PART => X);
    end case;
    SET_COL(F, POSITIVE_COUNT(PART_ENTRY_IO.DEFAULT_WIDTH)+C);
    return;
  end GET;

  procedure GET(P : out PART_ENTRY) is
    PS : PART_OF_SPEECH_TYPE := X;
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
      when VPAR =>
        null;                --  No VAPR entry
      when SUPINE =>
        null;                --  No SUPINE entry
      when PREP =>
        GET(PREPOSITION);
        P := (PREP, PREPOSITION);
      when CONJ =>
        GET(CONJUNCTION);
        P := (CONJ, CONJUNCTION);
      when INTERJ =>
        GET(INTERJECTION);
        P := (INTERJ, INTERJECTION);
      when PREFIX =>
        P := (PART => PREFIX);
      when SUFFIX =>
        P := (PART => SUFFIX);
      when TACKON =>
        P := (PART => TACKON);
      when X =>
        P := (PART => X);
    end case;
    return;
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in PART_ENTRY) is
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
      when VPAR =>
        null;                --  No VAPR entry
      when SUPINE =>
        null;                --  No SUPINE entry
      when PREP =>
        PUT(F, P.PREP);
      when CONJ =>
        PUT(F, P.CONJ);
      when INTERJ =>
        PUT(F, P.INTERJ);
      when others =>
        null;
    end case;
    PUT(F, STRING'((INTEGER(COL(F))..PART_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
   return;
  end PUT;


  procedure PUT(P : in PART_ENTRY) is
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
      when VPAR =>
        null;                --  No VAPR entry
      when SUPINE =>
        null;                --  No SUPINE entry
      when PREP =>
        PUT(P.PREP);
      when CONJ =>
        PUT(P.CONJ);
      when INTERJ =>
        PUT(P.INTERJ);
      when others =>
        null;
    end case;
    PUT(STRING'((INTEGER(COL)..PART_ENTRY_IO.DEFAULT_WIDTH+C-1 => ' ')));
    return;
  end PUT;

  procedure GET(S : in STRING; P : out PART_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
    PS : PART_OF_SPEECH_TYPE := X;
  begin
    GET(S, PS, L);
    LAST := L;      --  In case it is not set later
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
      when VPAR =>
        null;                --  No VAPR entry
      when SUPINE =>
        null;                --  No SUPINE entry
      when PREP =>
        GET(S(L+1..S'LAST), PREPOSITION, LAST);
        P := (PREP, PREPOSITION);
      when CONJ =>
        GET(S(L+1..S'LAST), CONJUNCTION, LAST);
        P := (CONJ, CONJUNCTION);
      when INTERJ =>
        GET(S(L+1..S'LAST), INTERJECTION, LAST);
        P := (INTERJ, INTERJECTION);
      when PREFIX =>
        P := (PART => PREFIX);
      when SUFFIX =>
        P := (PART => SUFFIX);
      when TACKON =>
        P := (PART => TACKON);
      when X =>
        P := (PART => X);
    end case;
  end GET;


  procedure PUT(S : out STRING; P : in PART_ENTRY) is
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
      when VPAR =>
        null;                --  No VAPR entryR
      when SUPINE =>
        null;                --  No SUPINE entry
      when PREP =>
        M := L + PREPOSITION_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PREP);
      when CONJ =>
        M := L + CONJUNCTION_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.CONJ);
      when INTERJ =>
        M := L + INTERJECTION_ENTRY_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.INTERJ);
      when others =>
        null;
    end case;
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end PART_ENTRY_IO;

package body TRANSLATION_RECORD_IO is
    use TEXT_IO;
    use AGE_TYPE_IO;
    use AREA_TYPE_IO;
    use GEO_TYPE_IO;
    use FREQUENCY_TYPE_IO;
    use SOURCE_TYPE_IO;
    SPACER : CHARACTER := ' ';
    LINE : STRING(1..250);
    LAST : INTEGER := 0;

    procedure GET(F : in TEXT_IO.FILE_TYPE; TR: out TRANSLATION_RECORD) is
    begin
      GET(F, TR.AGE);
      GET(F, SPACER);
      GET(F, TR.AREA);
      GET(F, SPACER);
      GET(F, TR.GEO);
      GET(F, SPACER);
      GET(F, TR.FREQ);
      GET(F, SPACER);
      GET(F, TR.SOURCE);
      GET(F, SPACER);
      GET_LINE(F, LINE, LAST);
      TR.MEAN := HEAD(LINE(1..LAST), MAX_MEANING_SIZE);
  end GET;

    procedure GET(TR : out TRANSLATION_RECORD) is
    begin
      GET(TR.AGE);
      GET(SPACER);
      GET(TR.AREA);
      GET(SPACER);
      GET(TR.GEO);
      GET(SPACER);
      GET(TR.FREQ);
      GET(SPACER);
      GET(TR.SOURCE);
      GET(SPACER);
      GET_LINE(LINE, LAST);
      TR.MEAN := HEAD(LINE(1..LAST), MAX_MEANING_SIZE);
    end GET;

    procedure PUT(F : in TEXT_IO.FILE_TYPE; TR : in TRANSLATION_RECORD) is
    begin
      PUT(F, TR.AGE);
      PUT(F, ' ');
      PUT(F, TR.AREA);
      PUT(F, ' ');
      PUT(F, TR.GEO);
      PUT(F, ' ');
      PUT(F, TR.FREQ);
      PUT(F, ' ');
      PUT(F, TR.SOURCE);
      PUT(F, ' ');
      PUT(F, TR.MEAN);
    end PUT;

    procedure PUT(TR : in TRANSLATION_RECORD) is
    begin
      AGE_TYPE_IO.PUT(TR.AGE);
      TEXT_IO.PUT(' ');
      AREA_TYPE_IO.PUT(TR.AREA);
      TEXT_IO.PUT(' ');
      GEO_TYPE_IO.PUT(TR.GEO);
      TEXT_IO.PUT(' ');
      FREQUENCY_TYPE_IO.PUT(TR.FREQ);
      TEXT_IO.PUT(' ');
      SOURCE_TYPE_IO.PUT(TR.SOURCE);
      TEXT_IO.PUT(' ');
      TEXT_IO.PUT(TR.MEAN);
    end PUT;

    procedure GET(S : in STRING; TR : out TRANSLATION_RECORD; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
      M : INTEGER := 0;
    begin
      M := L + AGE_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), TR.AGE, L);
      L := M + 1;
      M := L + AREA_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), TR.AREA, L);
      L := M + 1;
      M := L + GEO_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), TR.GEO, L);
      L := M + 1;
      M := L + FREQUENCY_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), TR.FREQ, L);
      L := M + 1;
      M := L + SOURCE_TYPE_IO.DEFAULT_WIDTH;
      GET(S(L+1..S'LAST), TR.SOURCE, L);
      L := M + 1;
      M := L + MAX_MEANING_SIZE;
      TR.MEAN := HEAD(S(L+1..S'LAST), MAX_MEANING_SIZE);
      LAST := M;
    end GET;

    procedure PUT(S : out STRING; TR : in TRANSLATION_RECORD) is
      L : INTEGER := 0;
      M : INTEGER := 0;
    begin
      M := L + AGE_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), TR.AGE);
      L := M + 1;
      S(L) :=  ' ';
      M := L + AREA_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), TR.AREA);
      L := M + 1;
      S(L) :=  ' ';
      M := L + GEO_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), TR.GEO);
      L := M + 1;
      S(L) :=  ' ';
      M := L + FREQUENCY_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), TR.FREQ);
      L := M + 1;
      S(L) :=  ' ';
      M := L + SOURCE_TYPE_IO.DEFAULT_WIDTH;
      PUT(S(L+1..M), TR.SOURCE);
      L := M + 1;
      S(L) :=  ' ';
      M := L + MAX_MEANING_SIZE;
      S(L+1..M) :=  TR.MEAN;
      S(M+1..S'LAST) := (others => ' ');
    end PUT;

    end TRANSLATION_RECORD_IO;



package body DICTIONARY_ENTRY_IO is
  use PART_ENTRY_IO;
  use TRANSLATION_RECORD_IO;

  SPACER : CHARACTER := ' ';

  DE : DICTIONARY_ENTRY;

  procedure GET(F : in FILE_TYPE; D : out DICTIONARY_ENTRY) is
  begin
   for I in 1..4  loop
      GET(F, D.STEMS(I));
      GET(F, SPACER);
    end loop;
    GET(F, D.PART);
    GET(F, SPACER);
    GET(F, D.TRAN);
   end GET;

  procedure GET(D : out DICTIONARY_ENTRY) is
  begin
   for I in 1..4  loop
      GET(D.STEMS(I));
      GET(SPACER);
    end loop;
    GET(D.PART);
    GET(SPACER);
    GET(D.TRAN);
   end GET;

  procedure PUT(F : in FILE_TYPE; D : in DICTIONARY_ENTRY) is
  begin
    for I in 1..4  loop
      PUT(F, D.STEMS(I));
      PUT(F, ' ');
    end loop;
    PUT(F, D.PART);
    PUT(F, ' ');
    PUT(F, D.TRAN);
   end PUT;

  procedure PUT(D : in DICTIONARY_ENTRY) is
  begin
    for I in 1..4  loop
      PUT(D.STEMS(I));
      PUT(' ');
    end loop;
    PUT(D.PART);
    PUT(' ');
    PUT(D.TRAN);
   end PUT;

  procedure GET(S : in STRING; D : out DICTIONARY_ENTRY; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    for I in 1..4  loop
      M := L + MAX_STEM_SIZE;
      D.STEMS(I) := S(L+1..M);
      L := L + 1;
    end loop;
    M := L + PART_ENTRY_IO.DEFAULT_WIDTH;
    GET(S(L+1..S'LAST), D.PART, L);
    L := L + 1;
    M := L + TRANSLATION_RECORD_IO.DEFAULT_WIDTH;
    GET(S(L+1..S'LAST), D.TRAN, L);
    LAST := M;
  end GET;

  procedure PUT(S : out STRING; D : in DICTIONARY_ENTRY) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    for I in 1..4  loop
      M := L + MAX_STEM_SIZE;
      S(L+1..M) := D.STEMS(I);
      L := M + 1;
      S(L) :=  ' ';
    end loop;
    M := L + PART_ENTRY_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), D.PART);
    L := M + 1;
    S(L) :=  ' ';
    M := M + TRANSLATION_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), D.TRAN);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;

end DICTIONARY_ENTRY_IO;





  function "<=" (LEFT, RIGHT : AREA_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
       RIGHT = X  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";


begin     --  initialization of body of DICTIONARY_PACKAGE
--TEXT_IO.PUT_LINE("Initializing DICTIONARY_PACKAGE");

  DICTIONARY_KIND_IO.DEFAULT_WIDTH := DICTIONARY_KIND'WIDTH;

  --NUMERAL_VALUE_TYPE_IO.DEFAULT_WIDTH := 5;

  AREA_TYPE_IO.DEFAULT_WIDTH := AREA_TYPE'WIDTH;

  GEO_TYPE_IO.DEFAULT_WIDTH := GEO_TYPE'WIDTH;

  FREQUENCY_TYPE_IO.DEFAULT_WIDTH := FREQUENCY_TYPE'WIDTH;

  SOURCE_TYPE_IO.DEFAULT_WIDTH := SOURCE_TYPE'WIDTH;

  AAMNPC_RECORD_IO.DEFAULT_WIDTH :=
                                     AGE_TYPE_IO.DEFAULT_WIDTH + 1 +
                                     AREA_TYPE_IO.DEFAULT_WIDTH + 1 +
                                     GEO_TYPE_IO.DEFAULT_WIDTH + 1 +
                                     FREQUENCY_TYPE_IO.DEFAULT_WIDTH + 1 +
                                     SOURCE_TYPE_IO.DEFAULT_WIDTH + 1 +
                                     6;


  PARSE_RECORD_IO.DEFAULT_WIDTH :=
                                   MAX_STEM_SIZE + 1 +
                                   INFLECTION_RECORD_IO.DEFAULT_WIDTH + 1 +
                                   DICTIONARY_KIND_IO.DEFAULT_WIDTH + 1 +
                                   6;
  NOUN_ENTRY_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   GENDER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
  PRONOUN_ENTRY_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
  PROPACK_ENTRY_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
  ADJECTIVE_ENTRY_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   COMPARISON_TYPE_IO.DEFAULT_WIDTH;
  ADVERB_ENTRY_IO.DEFAULT_WIDTH :=
                   COMPARISON_TYPE_IO.DEFAULT_WIDTH;
  VERB_ENTRY_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   VERB_KIND_TYPE_IO.DEFAULT_WIDTH;
  PREPOSITION_ENTRY_IO.DEFAULT_WIDTH := CASE_TYPE_IO.DEFAULT_WIDTH;
  CONJUNCTION_ENTRY_IO.DEFAULT_WIDTH := 0;
  INTERJECTION_ENTRY_IO.DEFAULT_WIDTH := 0;
  NUMERAL_ENTRY_IO.DEFAULT_WIDTH :=
                 DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                 NUMERAL_KIND_TYPE_IO.DEFAULT_WIDTH + 1 +
                 --NUMERAL_VALUE_TYPE_IO.DEFAULT_WIDTH;
                 5;

  PART_ENTRY_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                              NUMERAL_ENTRY_IO.DEFAULT_WIDTH; --  Largest



  TRANSLATION_RECORD_IO.DEFAULT_WIDTH :=
                                         AGE_TYPE_IO.DEFAULT_WIDTH + 1 +
                                         AREA_TYPE_IO.DEFAULT_WIDTH + 1 +
                                         GEO_TYPE_IO.DEFAULT_WIDTH + 1 +
                                         FREQUENCY_TYPE_IO.DEFAULT_WIDTH + 1 +
                                         SOURCE_TYPE_IO.DEFAULT_WIDTH + 1 +
                                         MAX_MEANING_SIZE;


  DICTIONARY_ENTRY_IO.DEFAULT_WIDTH := 4 * (MAX_STEM_SIZE + 1) +
                           PART_ENTRY_IO.DEFAULT_WIDTH + 1 +
                           MAX_MEANING_SIZE;

--TEXT_IO.PUT_LINE("Initialized  DICTIONARY_PACKAGE");

end DICTIONARY_PACKAGE;