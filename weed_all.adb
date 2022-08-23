with TEXT_IO; use TEXT_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
procedure WEED_ALL(W : in out STRING; 
                POFS : in PART_OF_SPEECH_TYPE) is
--  In contrast to the Latin phase where the prioritization takes is at runtime
--  for the English most of the work is done beforehand
--  both the setting of a priority class for each entry in the scan of DICTLINE
--  and the WEEDing/TRIMming done herein
      KILL : BOOLEAN := FALSE;
    begin     
--PUT_LINE("WEED    "  & W & '|');         
      if W'LENGTH > 0  then
    
      if   --  WORDS words
        W = "DECL" or
        W = "DAT"  or
        W = "ACC"  or
        W = "ABL"  or
        W = "ADJ"  or
        W = "AD"  or
        W = "BC"  or
        W = "COMP"  or
        W = "SUPER"  or
        W = "DEMONST"  or
        W = "INDEF"  or
        W = "INF"  or
        W = "KLUDGE"  or
        W = "NE"  or
        W = "NW"  or
        W = "SE"  or
        W = "SW"  or
        W = "NT"  or
        W = "OT"  or
        W = "PASS"  or
        W = "L+S"  or
        W = "St"        
      
                       then
            
        KILL := TRUE;
      end if;
      
      
      
      
      if 
           --  Articles
           W = "a"    or
           W = "an"   or
           W = "the"  or    
           W = "The"  or 
             
           --  Others  
           W = "no"   
                  
                         then
            
           KILL := TRUE;
         end if;
             
     
          
             


         if
           --  Abbreviations     
           W = "abb"   or
             --  Number suffixes   
           W = "th"   or
           W = "st"   or
           W = "nd"   
                          then
             KILL := TRUE;
  
                end if;
 
        --  Kill abbreviations
        if W(W'LAST) = '.'  then
          KILL := TRUE;
        end if; 
                          
        --  Kill internal AREA
        if W(W'LAST) = ':'  then
          KILL := TRUE;
        end if;                   
         
          if 
           W'LENGTH  <= 1  
                     then
            
           KILL := TRUE;
         end if;
             
     
                     
                
        if KILL then
          for I in W'RANGE  loop
            W(I) := '.';
          end loop;
        end if;
        end if;
   --PUT_LINE("WEEDed ANY  "  & W & '|' & BOOLEAN'IMAGE(KILL));         
     
end WEED_ALL;

