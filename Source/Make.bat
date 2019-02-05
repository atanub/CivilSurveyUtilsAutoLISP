rem @echo off
path=%path%;"C:\Projects\Utility\LISPUTIL"
C:
CD "\Projects\FieldSurvey\Survey.LSP"

REM *************************************
REM Clean Up PLAN........................
REM *************************************
del *.bak
del *.mnx
del *.mnc
del *.mnr
del *.mns

REM *************************************
REM for Bakup............................
REM *************************************
deltree /y Bak\*.*
copy *.lsp Bak
copy *.dcl Bak
copy *.mnu Bak

REM *************************************
REM for Distribution.....................
REM *************************************
deltree /y Dist\*.*
MKDIR Dist

copy SurvPlan.mnu DIST
copy Survplan14.mnu DIST
copy SurvPlan.dcl DIST
copy SurvPlan.lsp DIST\Junk01.lsp
copy SurvCS.lsp   DIST\Junk02.lsp
copy SurvPR12.lsp   DIST\Junk03.lsp
copy SurvUtil.lsp DIST\Junk04.lsp
copy SurvStns.lsp DIST\Junk05.lsp
copy SurvStnx.lsp DIST\Junk06.lsp
copy SurvPR14.lsp   DIST\Junk07.lsp
copy SurvQPrf.lsp   DIST\Junk08.lsp
copy OldSurvCS.lsp   DIST\Junk09.lsp

cd DIST
protect Junk01.lsp Atanu SurvPlan.lsp
protect Junk02.lsp Atanu SurvCS.lsp
protect Junk03.lsp Atanu SurvPR12.lsp
protect Junk04.lsp Atanu SurvUtil.lsp
protect Junk05.lsp Atanu SurvStns.lsp
protect Junk06.lsp Atanu SurvStnX.lsp
protect Junk07.lsp Atanu SurvPR14.lsp
protect Junk08.lsp Atanu SurvQPrf.lsp
protect Junk09.lsp Atanu OldSurvCS.lsp


deltree /y J*.*
CD "\Projects\FieldSurvey\Survey.LSP"

