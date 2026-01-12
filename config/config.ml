open Shared_api

let key_mappings ctx = [
    (* JMP Mode *)
    {
        key = "l";
        prefix = Some "k";
        action = (Act_KillLine ctx.current_line)::[];
        mode = Mode_Jmp;
    };
    {
        key = "x";
        prefix = Some "c";
        action = (Act_CenterLine ctx.current_line)::[];
        mode = Mode_Jmp;
    };
    {
        key = "l";
        prefix = Some "ESC";
        action = Act_EoL::[];
        mode = Mode_Jmp;
    };
    {
        key = "l";
        prefix = None;
        action = Act_BoL::[];
        mode = Mode_Jmp;
    };
    {
        key = "SPACE";
        prefix = Some "ESC";
        action = (Act_ModeSwitch Mode_Edt)::[];
        mode = Mode_Jmp;
    };
    {
        key = ";";
        prefix = Some "ESC";
        action = Act_VpShiftX::[];
        mode = Mode_Jmp;
    };
    {
        key = "i";
        prefix = Some "ESC";
        action = Act_ToggleStatus::[];
        mode = Mode_Jmp;
    };
    {
        key = "w";
        prefix = Some "ESC";
        action = Act_PageUp::[];
        mode = Mode_Jmp;
    };
    {
        key = "s";
        prefix = Some "ESC";
        action = Act_PageDown::[];
        mode = Mode_Jmp;
    };
    {
        key = "ENTER";
        prefix = Some "ESC";
        action = (Act_InsertLine (ctx.current_line - 1, "")) :: (Act_ModeSwitch Mode_Edt) :: [];
        mode = Mode_Jmp;
    };
    {
        key = "ENTER";
        prefix = None;
        action = (Act_InsertLine (ctx.current_line, "")) :: (Act_ModeSwitch Mode_Edt) :: [];
        mode = Mode_Jmp;
    };
    {
        key = ";";
        prefix = Some "SPACE";
        action = Act_ToBufferTop :: [];
        mode = Mode_Jmp;
    };
    {
        key = "\'";
        prefix = Some "SPACE";
        action = Act_ToBufferBottom :: [];
        mode = Mode_Jmp;
    };
    {
        key = "w";
        prefix = None;
        action = Act_MoveUp::[];
        mode = Mode_Jmp;
    };
    {
        key = "a";
        prefix = None;
        action = Act_MoveLeft::[];
        mode = Mode_Jmp;
    };
    {
        key = "s";
        prefix = None;
        action = Act_MoveDown::[];
        mode = Mode_Jmp;
    };
    {
        key = "d";
        prefix = None;
        action = Act_MoveRight::[];
        mode = Mode_Jmp;
    };
    {
        key = "i";
        prefix = None;
        action = Act_StatusI::[];
        mode = Mode_Jmp;
    };
    {
        key = "u";
        prefix = None;
        action = Act_Undo::[];
        mode = Mode_Jmp;
    };
    {
        key = ".";
        prefix = None;
        action = Act_RepLast::[];
        mode = Mode_Jmp;
    };
    {
        key = "k";
        prefix = None;
        action = (Act_Pending "k")::[];
        mode = Mode_Jmp;
    };
    {
        key = "c";
        prefix = None;
        action = (Act_Pending "c")::[];
        mode = Mode_Jmp;
    };
    {
        key = "SPACE";
        prefix = None;
        action = (Act_Pending "SPACE")::[];
        mode = Mode_Jmp;
    };
    {
        key = "ESC";
        prefix = None;
        action = (Act_Pending "ESC")::[];
        mode = Mode_Jmp;
    };

    (* EDT Mode *)
    {
        key = "SPACE";
        prefix = Some "ESC";
        action = (Act_ModeSwitch Mode_Jmp)::[];
        mode = Mode_Edt;
    };
    {
        key = "i";
        prefix = Some "ESC";
        action = Act_ToggleStatus::[];
        mode = Mode_Edt;
    };
    {
        key = "w";
        prefix = Some "ESC";
        action = Act_MoveUp::[];
        mode = Mode_Edt;
    };
    {
        key = "s";
        prefix = Some "ESC";
        action = Act_MoveDown::[];
        mode = Mode_Edt;
    };
    {
        key = "a";
        prefix = Some "ESC";
        action = Act_MoveLeft::[];
        mode = Mode_Edt;
    };
    {
        key = "d";
        prefix = Some "ESC";
        action = Act_MoveRight::[];
        mode = Mode_Edt;
    };
    {
        key = "ESC";
        prefix = None;
        action = (Act_Pending "ESC")::[];
        mode = Mode_Edt;
    }
]
