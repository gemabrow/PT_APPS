//----------------------------------------------------------------------------
// 
// Corresponding dialogue for DDNUMB.LSP
// 
//----------------------------------------------------------------------------

//dcl_settings : default_dcl_settings { audit_level = 3; }


ddstart : dialog {
     label = "Auto Number";
	initial_focus = "prefix";
		: row {
		: boxed_column {
			fixed_width = true;
			label = "&Prefix";
				: edit_box {
					key = "prefix";
					mnemonic = "P";
					fixed_width = true;
		  	 		}
				}
		: boxed_column {
			fixed_width = true;
			label = "Start &No";
				: edit_box {
					key = "start_no";
					mnemonic = "N";
					fixed_width = true;
		  	 		}
				}
		: boxed_column {
			fixed_width = true;
			label = "&Suffix";
				: edit_box {
					key = "suffix";
					mnemonic = "S";
					fixed_width = true;
			   		}
				}
			}
	: row {
		: boxed_column {
			fixed_width = true;
			label = "&Increment";
				: edit_box {
					key = "txt_inc";
					mnemonic = "I";
					fixed_width = true;
			   		}
				}
		: boxed_column {
			fixed_width = true;
			label = "Sort &By";
				: popup_list {
					key = "sort_type";
					width = 13;
					fixed_width = true;
					}
				}
		: toggle {
			label = "Replace";
			key = "do_replace";
			value = 1;
			}
		}
		spacer_1;
		: row {
	ok_cancel;
			: button {
				key = "numb_hlp";
				label = "Help";
				mnemonic = "H";
				}
			}
		}

