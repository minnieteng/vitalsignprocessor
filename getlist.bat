del /f list.txt
for /f %%i in ('FORFILES /p C:\DATA /m OR*.txt /C "cmd /c if @fsize gtr 10000 echo @path"') do @echo %%~i >> list.txt
