files were generated from the original HLApp_user_guide.docx file

-HTML - for in app user guide
pandoc HLApp_user_guide.docx   -o HLApp_user_guide.html   --standalone   --embed-resources

-PDF - for downloadable content
saved as pdf in LibreOffice Writer

-MD - for github readme
pandoc HLA_Promiscuity_App_User_Guide_final.docx -o HLApp_user_guide.md -t gfm
then file content was copied into README.md