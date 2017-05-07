to_cmprble = function(string){
KlAstate = F #Klammer auf - state  / parenthesis open - state
gKlAstate = F #geschweifte Klammer auf - state / curly bracket open - state
for(i in 1:nchar(string)){
  if(substr(string,i,i)==")"){ substr(string,i,i) = " "; KlAstate = F; next}
  if(substr(string,i,i)=="}"){ substr(string,i,i) = " "; gKlAstate= F; next}

  if( KlAstate == T ){ substr(string,i,i) = " "; next}
  if( gKlAstate == T){ substr(string,i,i) = " "; next}
  
  if(substr(string,i,i)=="("){ substr(string,i,i) = " "; KlAstate = T; next}
  if(substr(string,i,i)=="{"){ substr(string,i,i) = " "; gKlAstate= T; next}
  
  if(substr(string,i,i) %in% c("?","!","+","#","\n"," ")){substr(string,i,i)=" "; next}
}

#ersetze alle Ausdrücke der Form: [1 und mehr Ziffern gefolgt von ...] durch "/" 
#replace all expressions of the form: [1 and more digits followed by ...] by "/"
string=gsub("\\d+(\\.{3})","/",string)

string=gsub("...","/",string,fixed=T)
string=gsub("1-0","/",string,fixed=T)
string=gsub("0-1","/",string,fixed=T)
string=gsub("1/2-1/2","/",string,fixed=T)
string=gsub(" ","/",string,fixed=T)

#ersetze alle Audrücke der Form: [1 und mehr /] durch "/"
#replace all expressions of the form: [1 and more /] by "/"
string=gsub("/+","/",string)

#ersetze das "/" am Anfang vom string sofern vorhanden
#replace "/" at the beginning of the string if existent
string=gsub("^/","",string) 

#ersetze das "/" am Ende vom string sofern vorhanden
#replace "/" at the end of the string if existent
string=gsub("/$","",string)
  
string
}