with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE; use ADDONS_PACKAGE;
package LATIN_DEBUG is
  
    procedure PUT(ITEM : CHARACTER);
    procedure PUT(ITEM : STRING);
    procedure PUT_LINE(ITEM : STRING);
    procedure NEW_LINE(N : POSITIVE := 1);
    procedure SET_COL(TO : POSITIVE);

    procedure PUT(ITEM : BOOLEAN);
    procedure PUT(ITEM : INTEGER);
    procedure PUT(ITEM : PART_OF_SPEECH_TYPE);
    procedure PUT(ITEM : QUALITY_RECORD);
    procedure PUT(ITEM : DECN_RECORD);
    procedure PUT(ITEM : COMPARISON_TYPE);
    procedure PUT(ITEM : PRONOUN_KIND_TYPE);   
    

    procedure PUT(ITEM : AGE_TYPE);
    procedure PUT(ITEM : AREA_TYPE);   
    
    procedure PUT(ITEM : PART_ENTRY);
    procedure PUT(ITEM : INFLECTION_RECORD);  
--procedure PUT(ITEM : DICTIONARY_ENTRY);   
    procedure PUT(ITEM : DICTIONARY_KIND); 
    procedure PUT(ITEM : PARSE_RECORD);      
    procedure PUT(ITEM : DICT_IO.POSITIVE_COUNT); 
    
    procedure PUT(ITEM : SUFFIX_ENTRY);
  
end LATIN_DEBUG;
