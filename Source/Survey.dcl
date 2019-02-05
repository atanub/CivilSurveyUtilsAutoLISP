//
//SurvPlan.DCL
//
//
// Plan Data =========================================================
//
GetSurvPlanData : dialog {
   label = "Survey Plan Parameters";
   : row { : column { : button { label = "Sheet Configuration File..."; key = "ShConfFileNameButt"; mnemonic = "S";} : button { label = "Object Configuration File..."; key = "ObConfFileNameButt"; mnemonic = "b";} : button { label = "Object Data File..."; key = "ObDataFileNameButt"; mnemonic = "D";} : button { label = "Spot Level Data File..."; key = "SpLevDataFileNameButt"; mnemonic = "p";}} : column { : edit_box { key = "ShConfFileNameEBox"; width = 20;} : edit_box { key = "ObConfFileNameEBox"; width = 20;} : edit_box { key = "ObDataFileNameEBox"; width = 20;} : edit_box { key = "SpLevFileNameEBox"; width = 20;}}}
   : boxed_row
   {
     label = "Spot level data type";
     mnemonic = "p";
	: radio_button { label = "2 Dimensional"; key = "SpotLevelDataIn2D"; mnemonic = "2";value = "1"; }
	: radio_button { label = "3 Dimensional"; key = "SpotLevelDataIn3D"; mnemonic = "3";}
   }
   : edit_box { label = "Point Marker Block :"; key = "PtMarkBlk"; width = 10; mnemonic = "M";}
   : row { : spacer {width = 1;} : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";} : button { label= "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";} : spacer {width = 1;}}
   : text { label = ""; key = "err"; width = 20;}
}
//
// StnDetail Data =========================================================
//"StnDatFileNameButt" "PIDatFileNameButt"
//"StnDatFileEBox" "PIDatFileNameEBox"  "NumAnnTxtSizeEB" "LabAnnTxtSizeEB" "LabTxtEB"
//
GetStnDetailData : dialog {
   label = "Station Detailing Parameters";
   : row { : column { : button { label = "Station Data File..."; key = "StnDatFileNameButt"; mnemonic = "S";} : button { label = "PI Data File..."; key = "PIDatFileNameButt"; mnemonic = "P";} : button { label = "Output File..."; key = "OutDatFileNameButt"; mnemonic = "O";}} : column { : edit_box { key = "StnDatFileEBox"; width = 20;} : edit_box { key = "PIDatFileNameEBox"; width = 20;} : edit_box { key = "OutDatFileNameEBox"; width = 20;}}}
   : edit_box { label = "Label Text :"; key = "LabTxtEB"; width = 10; mnemonic = "T";}
   : boxed_row {
  	 label = "Annotation Sizes";
	 : edit_box { label = "Numeric :"; key = "NumAnnTxtSizeEB"; width = 10; mnemonic = "N";}
	 : edit_box { label = "Label :"; key = "LabAnnTxtSizeEB"; width = 10; mnemonic = "L";}
   }
   : edit_box { label = "Block name as Point mark :"; key = "BlkNameEB"; width = 10; mnemonic = "B";}
   : row { : spacer {width = 1;} : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";} : button { label= "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";} : spacer {width = 1;}}
   : text { label = ""; key = "err"; width = 20;}
}

// Cross Section Data =========================================================
//
GetCSData : dialog 
{
   label = "Cross Section Data";
   : row {
     : button { label = "Select Data File..."; mnemonic  = "S"; key = "AlgnNameButton";}
     : edit_box { key = "AlgnName"; width = 6;}
   }
   : row {
	   : boxed_row {
			label = "Offset";
			: column {
			  : text { label = "Left :"; alignment = left;}
			  : text { label = "Right :"; alignment = left;}
			}
			: column {
			 : edit_box { key = "MinOff"; alignment = left; mnemonic  = "M"; fixed_width = true;}
			 : edit_box { key = "MaxOff"; alignment = left; mnemonic  = "X"; fixed_width = true;}
			}
 	   }
	   : boxed_row {
			label = "Exaggeration";
			: column {
			  : text { label = "Horz.:"; alignment = left;}
			  : text { label = "Vert.:"; alignment = left;}
			}
			: column {
			 : edit_box { key = "HorSca"; alignment = left; mnemonic  = "M"; fixed_width = true;}
			 : edit_box { key = "VerSca"; alignment = left; mnemonic  = "X"; fixed_width = true;}
			}
 	   }
  }
  : row {
	   : column {
	     : boxed_row {
		   label = "Draw Stations";
		   : column {
			   : text { label = "From:"; alignment = left;}
			   : text { label = "Upto:"; alignment = left;}
		   }
		   : column {
			   : edit_box { key = "FromStn"; alignment = left; mnemonic  = "H"; fixed_width = true;}
			   : edit_box { key = "UptoStn"; alignment = left; mnemonic  = "V"; fixed_width = true;}
		   }
	     }
	     : button { label = "Annotation..."; key = "AnnTextButt"; mnemonic  = "A";}
	     : button { label = "Graph Position..."; key = "GraphPosnButt"; mnemonic  = "G";}
	   }
	   : boxed_column {
		   label = "Start Point";
		   : edit_box { label = "X:"; key = "OrdX"; width = 3; alignment = left; mnemonic  = "V"; }
		   : edit_box { label = "Y:"; key = "OrdY"; width = 3; alignment = left; mnemonic  = "V"; }
		   : edit_box { label = "Z:"; key = "OrdZ"; width = 3; alignment = left; mnemonic  = "V"; }
		   : button { label = "Point <"; key = "PointButt"; mnemonic  = "P";}
	   }
   }
   : row {
     : spacer {width = 1;}
     : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";}
     : button { label = "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";}
     : spacer {width = 1;}
   }
   : text { label = ""; key = "err"; width = 15;}
}
GetCSDataAnnotation : dialog
{
   label = "Annotation Detail";
   : row {
	   : boxed_row {
 		   label = "Annotation Text Size (Plot)";
		   : column {
			 : text { label = "Numerics:"; alignment = left; fixed_width = true;}
			 : text { label = "Labels:"; alignment = left; fixed_width = true;}
			 : text { label = "Station Labels:"; alignment = left; fixed_width = true;}
		   }
		   : column {
			 : edit_box { key = "NumAnnSize"; alignment = left; mnemonic  = "N";}
			 : edit_box { key = "LabelAnnSize"; alignment = left; mnemonic  = "K";}
			 : edit_box { key = "StnAnnSize"; alignment = left; mnemonic  = "S";}
		   }
	   }
	   : boxed_row{
		 label = "Annotation Labels for";
		   : column {
			 : text { label = "Chainage:"; alignment = left; fixed_width = true;}
			 : text { label = "Level Caption:"; alignment = left; fixed_width = true;}
			 : text { label = "C/Section Title :"; alignment = left; fixed_width = true;}
		   }
		   : column {
			 : edit_box { key = "LabelChain"; width = 20; alignment = left; mnemonic  = "M"; fixed_width = true;}
			 : edit_box { key = "LabelLevel"; width = 20; alignment = left; mnemonic  = "M"; fixed_width = true;}
			 : edit_box { key = "LabelStation"; width = 20; alignment = left; mnemonic  = "X"; fixed_width = true;}
		   }
	   }
   }
   : row {
     : edit_box { label = "Datum Level in Metre :"; key = "DatumLevel"; width = 10; alignment = left; mnemonic  = "M"; fixed_width = true;}
     //: spacer {width = 1;}
     : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";}
     : button { label = "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";}
     //: spacer {width = 1;}
   }
   : text { label = ""; key = "err"; width = 20;}
}
GetCSDataGraph : dialog
{
   label = "Draw Graph Parameters";
   : boxed_row {
     label = "No. Of Graph Along";
     : edit_box { label = "Vertical:"; key = "HorGrphNo"; alignment = left; mnemonic  = "H";}
     : edit_box { label = "Horizontal:"; key = "VerGrphNo"; alignment = left; mnemonic  = "V";}
   }
   : row {
     : spacer {width = 1;}
     : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";}
     : button { label = "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";}
     : spacer {width = 1;}
   }
   : text { label = ""; key = "err"; width = 20;}
}
//
// Profile Drawing =========================================================
//
//
GetProfileData : dialog 
{
   label = "Profile Draw Data";
   : row {
     : button { label = "Select Data File..."; mnemonic  = "S"; key = "ProfNameButton";}
     : edit_box { key = "ProfileName"; width = 6;}
   }
   : row {
	   : boxed_row {
			label = "Elevation";
			: column {
			  : text { label = "Datum :"; alignment = left;}
			  : text { label = "Maxm. :"; alignment = left;}
			}
			: column {
			 : edit_box { key = "MinElev"; alignment = left; mnemonic  = "D"; fixed_width = true;}
			 : edit_box { key = "MaxElev"; alignment = left; mnemonic  = "X"; fixed_width = true;}
			}
 	   }
	   : boxed_row {
			label = "Exaggeration";
			: column {
			  : text { label = "Horz.:"; alignment = left;}
			  : text { label = "Vert.:"; alignment = left;}
			}
			: column {
			 : edit_box { key = "HorSca"; alignment = left; mnemonic  = "M"; fixed_width = true;}
			 : edit_box { key = "VerSca"; alignment = left; mnemonic  = "X"; fixed_width = true;}
			}
 	   }
  }
  : column {
	   : boxed_row {
		   label = "Start Point";
		   : edit_box { label = "X:"; key = "OrdX"; width = 2; alignment = left; mnemonic  = "V"; }
		   : edit_box { label = "Y:"; key = "OrdY"; width = 2; alignment = left; mnemonic  = "V"; }
		   : edit_box { label = "Z:"; key = "OrdZ"; width = 2; alignment = left; mnemonic  = "V"; }
		   : button { label = "Point <"; key = "PointButt"; mnemonic  = "P";}
	   }
       : row {
           : boxed_row {
               label = "Draw Profiles ";
               key = "BoxedRowStnFrTo";
	           : edit_box { label = "From:"; key = "FromStn"; alignment = left; mnemonic  = "H"; fixed_width = true;}
	           : edit_box { label = "Upto:"; key = "UptoStn"; alignment = left; mnemonic  = "V"; fixed_width = true;}
            }
           : row {
                : spacer {height = 1;}
                : button { label = "Annotation..."; key = "AnnTextButt"; mnemonic  = "A";}
                : spacer {height = 1;}
           }
       }
   }
   : row {
     : spacer {width = 1;}
     : button { label = "  Ok  "; key = "accept"; is_default = true; fixed_width = true; mnemonic  = "O";}
     : button { label = "Cancel"; key = "cancel"; is_cancel  = true; fixed_width = true; mnemonic  = "C";}
     : spacer {width = 1;}
   }
   : text { label = ""; key = "err"; width = 15;}
}

GetProfileType : dialog {
   label = "Profile Data Content";
   : row {
	: radio_button { label = "Chainage"; key = "ChainageButt"; mnemonic = "C";}
	: radio_button { label = "Northing - Easting"; key = "NEButt"; mnemonic = "N";}
   }
   : row { : spacer {width = 1;} : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";} : button { label= "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";} : spacer {width = 1;}}
}
GetXDataType : dialog {
   label = "Modify Annotations";
   : boxed_row {
	label = "Annotation Type";
	: radio_button { label = "Elevation"; key = "ElevButt"; mnemonic = "E";}
	: radio_button { label = "Chainage"; key = "ChainageButt"; mnemonic = "C";}
	: radio_button { label = "Northing"; key = "NButt"; mnemonic = "N";}
	: radio_button { label = "Easting"; key = "EButt"; mnemonic = "A";}
   }
   : boxed_row {
       label = "Add Values";
	   : column {
		   : edit_box { label = "Elevation :"; key = "ElevEBox"; alignment = left; mnemonic  = "V"; fixed_width = true;}
		   : edit_box { label = "Chainage :"; key = "ChainageEBox"; alignment = left; mnemonic  = "H"; fixed_width = true;}
		   : spacer {height = 1;}
	   }
	   : boxed_column {
		   label = "Northing-Easting";
		   : edit_box { label = "Northing :"; key = "NEBox"; alignment = left; mnemonic  = "r"; fixed_width = true;}
		   : edit_box { label = "Easting :"; key = "EEBox"; alignment = left; mnemonic  = "a"; fixed_width = true;}
	   }
    }
   : row { : spacer {width = 1;} : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";} : button { label= "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";} : spacer {width = 1;}}
}
//
// Insertion of Multiple blocks =========================================================
//
GetMultiBlkInsData : dialog 
{
   label = "Multiple Block Insertion";
   : row {
     : button { label = "Select Data File..."; mnemonic  = "S"; key = "FileButt";}
     : edit_box { key = "FileEBox"; width = 6;}
   }
   : toggle { label = "Northing-Easting Data with Elevation"; key = "ZValFlagTogg"; mnemonic = "Z";}
   : edit_box { label = "Block Name :"; key = "BlkEbox"; alignment = left; mnemonic  = "B"; }
   : row {
     : spacer {width = 1;}
     : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";}
     : button { label = "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";}
     : spacer {width = 1;}
   }
   : text { label = ""; key = "err"; width = 15;}
}


//
// Insert Points =========================================================
//
GetInsPointsData : dialog
{
   label = "Draw Points";
   : row
   {
     : button { label = "Select Data File..."; mnemonic  = "S"; key = "FileButt";}
     : edit_box { key = "FileEBox"; width = 16;}
   }
   : edit_box { label = "Delimeter character :"; key = "Delimeter"; width = 16;}
   : toggle { label = "Consider Z-Value while drawing"; key = "ZValFlagTogg"; mnemonic = "Z";}
   : row { : spacer {width = 1;} : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";} : button { label= "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";} : spacer {width = 1;}}
   : text { label = ""; key = "err"; width = 20;}
}
//
// StnDetail Data ADS =========================================================
GetStnDetailDataEx : dialog
{
   label = "Station Detailing Parameters";
   : row 
   {
		: column
		{ 
			: button { label = "Master Data File..."; key = "MasDatFileNameButt"; mnemonic = "M";}
			: button { label = "Station Data File..."; key = "StnDatFileNameButt"; mnemonic = "S";}
			: button { label = "PI Data File..."; key = "PIDatFileNameButt"; mnemonic = "P";}
		}
		: column
		{
			: edit_box { key = "MasDatFileNameEB"; width = 20;}
			: edit_box { key = "StnDatFileEB"; width = 20;} 
			: edit_box { key = "PIDatFileNameEB"; width = 20;} 
		}
   }
   : boxed_row
   {
		label = "Annotation attributes";
		: column
		{ 
			 : text { label = "Text Size :"; key = "T1"; mnemonic = "T";}
			 : text { label = "Point Block Size:"; key = "T2"; mnemonic = "B";}
			 : text { label = "Block Name (for Point Mark):"; key = "T3"; mnemonic = "N";}
			 : text { label = "Layer Name:"; key = "T4"; mnemonic = "L";}
		}
		: column
		{ 
			 : edit_box { label = ""; key = "TxtEB"; width = 15;}
			 : edit_box { label = ""; key = "BlkSizeEB"; width = 15;}
			 : edit_box { label = ""; key = "BlkName"; width = 15;}
			 : edit_box { label = ""; key = "LyrName"; width = 15;}
		}
   }
   : row { : spacer {width = 1;} : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";} : button { label= "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";} : spacer {width = 1;}}
   : text { label = ""; key = "err"; width = 20;}
}
// Cross Section Data (MFC) =========================================================
//
GetCSDataMFC : dialog 
{
   label = "Cross Section Data";
   : row {
	   : boxed_row {
			label = "Exaggeration";
			 : edit_box { label = "Horz.:"; key = "HorSca"; alignment = left; mnemonic  = "O"; fixed_width = true;}
			 : edit_box { label = "Vert.:"; key = "VerSca"; alignment = left; mnemonic  = "t"; fixed_width = true;}
 	   }
  }
  : row {
	   : column {
		   : boxed_column {
				 label = "Graph Position";
				 : edit_box { label = "Horz.:"; key = "VerGrphNo"; alignment = left; mnemonic  = "H";}
				 : edit_box { label = "Vert.:"; key = "HorGrphNo"; alignment = left; mnemonic  = "V";}
				 : spacer {height = 1;}
		   }
	     : button { label = "Annotation..."; key = "AnnTextButt"; mnemonic  = "A";}
	   }
	   : boxed_column {
		   label = "Start Point";
		   : edit_box { label = "X:"; key = "OrdX"; width = 3; alignment = left; mnemonic  = "X"; }
		   : edit_box { label = "Y:"; key = "OrdY"; width = 3; alignment = left; mnemonic  = "Y"; }
		   : edit_box { label = "Z:"; key = "OrdZ"; width = 3; alignment = left; mnemonic  = "Z"; }
		   : button { label = "Point <"; key = "PointButt"; mnemonic  = "P";}
	   }
   }
   : row {
     : spacer {width = 1;}
     : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";}
     : button { label = "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";}
     : spacer {width = 1;}
   }
   : text { label = ""; key = "err"; width = 15;}
}
GetCSDataAnnotationMFC : dialog
{
   label = "Annotation Detail";
   : row {
	   : column {
		   : boxed_row {
 			   label = "Annotation Text Size";
			   : column {
				 : text { label = "Numerics:"; alignment = left; fixed_width = true;}
				 : text { label = "Labels:"; alignment = left; fixed_width = true;}
				 : text { label = "Station Labels:"; alignment = left; fixed_width = true;}
				 : spacer {height = 1;}
			   }
			   : column {
				 : edit_box { key = "NumAnnSize"; alignment = left; mnemonic  = "N";}
				 : edit_box { key = "LabelAnnSize"; alignment = left; mnemonic  = "L";}
				 : edit_box { key = "StnAnnSize"; alignment = left; mnemonic  = "S";}
				 : spacer {height = 1;}
			   }
		   }
		   : edit_box { label = "Datum Level in Metre :"; key = "DatumLevel"; alignment = left; mnemonic  = "D";}
		}
	   : boxed_row{
		 label = "Annotation Labels for";
		   : column {
			 : text { label = "Chainage:"; alignment = left; fixed_width = true;}
			 : text { label = "Level Caption:"; alignment = left; fixed_width = true;}
			 : text { label = "Northing Caption :"; alignment = left; fixed_width = true;}
			 : text { label = "Easting Caption :"; alignment = left; fixed_width = true;}
			 : text { label = "C/Section Title :"; alignment = left; fixed_width = true;}
		   }
		   : column {
			 : edit_box { key = "LabelChain"; width = 20; alignment = left; mnemonic  = "M"; fixed_width = true;}
			 : edit_box { key = "LabelLevel"; width = 20; alignment = left; mnemonic  = "M"; fixed_width = true;}
			 : edit_box { key = "LabelNorthing"; width = 20; alignment = left; mnemonic  = "X"; fixed_width = true;}
			 : edit_box { key = "LabelEasting"; width = 20; alignment = left; mnemonic  = "X"; fixed_width = true;}
			 : edit_box { key = "LabelStation"; width = 20; alignment = left; mnemonic  = "X"; fixed_width = true;}
		   }
	   }
   }
   : row {
     : spacer {width = 1;}
     : button { label = "  Ok  "; key = accept; is_default = true; fixed_width = true; mnemonic  = "O";}
     : button { label = "Cancel"; key = cancel; is_cancel  = true; fixed_width = true; mnemonic  = "C";}
     : spacer {width = 1;}
   }
   : text { label = ""; key = "err"; width = 15;}
}
