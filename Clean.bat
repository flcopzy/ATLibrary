@echo off
  
color F2
  
echo {******************************************************} 
echo {  This file is part of delphi auxiliary toolkit,      }
echo {  its purpose is to clear all the lib temp files.     }
echo {******************************************************}  
echo;

attrib -h -s /s Thumbs.db >nul

echo ready for cleanning, please wait... 
echo;

echo deleting the temp files...(1/2)   
del /a /s /q *.dcu *.ddp *.dsk *.~* *.cfg *.o Thumbs.db *.map *.elf *.identcache *.local *.log *.exe *.dll >nul 2>nul 
echo temp files cleaned. 
echo;

echo deleting the temp dirs ...(2/2) 
for /r %%i in (.) do rd /q /s "%%i\__history" >nul 2>nul
echo temp dirs cleaned. 
echo;

echo clean completed! 

pause
 