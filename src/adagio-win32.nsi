;NSIS Modern User Interface version 1.63
;MultiLanguage Example Script
;Written by Joost Verburg
;Adapted for Adagio by A. Mosteo

!define MUI_PRODUCT "Adagio" ;Define your own software name here
!define MUI_VERSION "2.1.e"  ;Define your own software version here

!include "MUI.nsh"

;--------------------------------
;Configuration

  ;General
  OutFile "adagio-bin-win32-${MUI_VERSION}.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
  
  ;Remember install folder
  InstallDirRegKey HKCU "Software\TruliSoft\${MUI_PRODUCT}" ""
  
  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKCU" 
  !define MUI_LANGDLL_REGISTRY_KEY "Software\Trulisoft\${MUI_PRODUCT}" 
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"

;--------------------------------
;Modern UI Configuration

  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTSPAGE
  !define MUI_DIRECTORYPAGE
  !define MUI_FINISHPAGE
  !define MUI_FINISHPAGE_SHOWREADME README.txt
  
  !define MUI_ABORTWARNING
  
  !define MUI_UNINSTALLER
  !define MUI_UNCONFIRMPAGE

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"
  !insertmacro MUI_LANGUAGE "Spanish"
  
;--------------------------------
;Language Strings
    
  ;Descriptions
  LangString DESC_Adagio ${LANG_ENGLISH} "Adagio core components and GUI monitor"
  LangString DESC_Adagio ${LANG_SPANISH} "Componentes necesarios de Adagio e interfaz gráfica de monitorización"
  LangString DESC_Shortcuts ${LANG_ENGLISH} "Shortcuts to Adagio components"
  LangString DESC_Shortcuts ${LANG_SPANISH} "Enlaces de acceso a los componentes de Adagio"
  
;--------------------------------
;Data
  
  LicenseData /LANG=${LANG_ENGLISH} "resource\License [en].txt"
  LicenseData /LANG=${LANG_SPANISH} "resource\License [es].txt"

;--------------------------------
;Reserve Files
  
  ;Things that need to be extracted on first (keep these lines before any File command!)
  ;Only useful for BZIP2 compression
  !insertmacro MUI_RESERVEFILE_LANGDLL
  
;--------------------------------
;Installer Sections

Section "Adagio" Adagio

  ;ADD YOUR OWN STUFF HERE!

  SectionIn RO

  SetOutPath "$INSTDIR"
  File /r ".\obj_install\*.*"
  File ".\resource\adagio.ico"
  
  ;Store install folder
  WriteRegStr HKCU "Software\TruliSoft\${MUI_PRODUCT}" "" $INSTDIR
  
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
SectionEnd

Section "Start Menu Shortcuts" Shortcuts

  CreateDirectory "$SMPROGRAMS\Adagio"
  CreateShortCut "$SMPROGRAMS\Adagio\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe"
  CreateShortCut "$SMPROGRAMS\Adagio\Adagio.lnk" "$INSTDIR\Adagio.exe" "" "$INSTDIR\Adagio.exe" 
;  CreateShortCut "$SMPROGRAMS\Adagio\Adagio Java Console.lnk" "$INSTDIR\Agiomon.jar" "" "$INSTDIR\Adagio.exe" 
  CreateShortCut "$SMPROGRAMS\Adagio\Adagio Web Monitor.lnk" "http://127.0.0.1:24444" "" "$INSTDIR\Adagio.exe" 
  CreateShortCut "$SMPROGRAMS\Adagio\Adagio Configuration Helper.lnk" "http://127.0.0.1:24444/config.html" "" "$INSTDIR\Adagio.exe" 
  CreateShortCut "$SMPROGRAMS\Adagio\Adagio config file (adagio.xml).lnk" "$INSTDIR\adagio.xml" "" "$INSTDIR\adagio.xml"
  CreateShortCut "$SMPROGRAMS\Adagio\Readme.lnk" "$INSTDIR\README.txt" "" "$INSTDIR\README.txt"
  CreateShortCut "$SMPROGRAMS\Adagio\Sample files.lnk" "$INSTDIR\conf_samples" "" "$INSTDIR\conf_samples"
  
SectionEnd
  
;Display the Finish header
;Insert this macro after the sections if you are not using a finish page
!insertmacro MUI_SECTIONS_FINISHHEADER

;--------------------------------
;Installer Functions

Function .onInit

  !insertmacro MUI_LANGDLL_DISPLAY

FunctionEnd

;--------------------------------
;Descriptions

!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${Adagio} $(DESC_Adagio)
  !insertmacro MUI_DESCRIPTION_TEXT ${Shortcuts} $(DESC_Shortcuts)
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!

  RMDir /r "$INSTDIR"
  RMDir /r "$SMPROGRAMS\Adagio"

  DeleteRegKey HKCU "Software\TruliSoft\${MUI_PRODUCT}"

  ;Display the Finish header
  !insertmacro MUI_UNFINISHHEADER

SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onInit

  ;Get language from registry
  ReadRegStr $LANGUAGE HKCU "Software\TruliSoft\${MUI_PRODUCT}" "Installer Language"
  
FunctionEnd
