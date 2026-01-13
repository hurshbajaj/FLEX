[@@@warning "-26-27-32-33-21-69-37-34"]
open Shared_api

let fetch_theme = "/home/hursh/ocaml_ml/flex/queries/theme.json"

let fetch_keymappings = [
    (* JMP Mode *)
    {
        key = "l";
        prefix = Some "k";
        action = (fun ctx -> (Act_KillLine ctx.current_line));
        mode = Mode_Jmp;
    };
    {
        key = "x";
        prefix = Some "c";
        action = (fun ctx -> (Act_CenterLine ctx.current_line));
        mode = Mode_Jmp;
    };
    {
        key = "l";
        prefix = Some "ESC";
        action = (fun ctx -> Act_EoL);
        mode = Mode_Jmp;
    };
    {
        key = "l";
        prefix = None;
        action = (fun ctx -> Act_BoL);
        mode = Mode_Jmp;
    };
    {
        key = "SPACE";
        prefix = Some "ESC";
        action = (fun ctx -> (Act_ModeSwitch Mode_Edt));
        mode = Mode_Jmp;
    };
    {
        key = ";";
        prefix = Some "ESC";
        action = (fun ctx -> Act_VpShiftX);
        mode = Mode_Jmp;
    };
    {
        key = "i";
        prefix = Some "ESC";
        action = (fun ctx -> Act_ToggleStatus);
        mode = Mode_Jmp;
    };
    {
        key = "w";
        prefix = Some "ESC";
        action = (fun ctx -> Act_PageUp);
        mode = Mode_Jmp;
    };
    {
        key = "s";
        prefix = Some "ESC";
        action = (fun ctx -> Act_PageDown);
        mode = Mode_Jmp;
    };
    {
        key = "ENTER";
        prefix = Some "ESC";
        action = (fun ctx -> Act_Seq ((Act_InsertLine (ctx.current_line - 1, "")) :: (Act_ModeSwitch Mode_Edt) :: [] ));
        mode = Mode_Jmp;
    };
    {
        key = "ENTER";
        prefix = None;
        action = (fun ctx -> Act_Seq ( (Act_InsertLine (ctx.current_line, "")) :: (Act_ModeSwitch Mode_Edt) :: [] ));
        mode = Mode_Jmp;
    };
    {
        key = ";";
        prefix = Some "SPACE";
        action = (fun ctx -> Act_ToBufferTop);
        mode = Mode_Jmp;
    };
    {
        key = "\'";
        prefix = Some "SPACE";
        action = (fun ctx -> Act_ToBufferBottom);
        mode = Mode_Jmp;
    };
    {
        key = "w";
        prefix = None;
        action = (fun ctx -> Act_MoveUp);
        mode = Mode_Jmp;
    };
    {
        key = "a";
        prefix = None;
        action = (fun ctx -> Act_MoveLeft);
        mode = Mode_Jmp;
    };
    {
        key = "s";
        prefix = None;
        action = (fun ctx -> Act_MoveDown);
        mode = Mode_Jmp;
    };
    {
        key = "d";
        prefix = None;
        action = (fun ctx -> Act_MoveRight);
        mode = Mode_Jmp;
    };
    {
        key = "i";
        prefix = None;
        action = (fun ctx -> Act_StatusI);
        mode = Mode_Jmp;
    };
    {
        key = "u";
        prefix = None;
        action = (fun ctx -> Act_Undo);
        mode = Mode_Jmp;
    };
    {
        key = ".";
        prefix = None;
        action = (fun ctx -> Act_RepLast);
        mode = Mode_Jmp;
    };
    {
        key = "k";
        prefix = None;
        action = (fun ctx -> (Act_Pending "k"));
        mode = Mode_Jmp;
    };
    {
        key = "c";
        prefix = None;
        action = (fun ctx -> (Act_Pending "c"));
        mode = Mode_Jmp;
    };
    {
        key = "SPACE";
        prefix = None;
        action = (fun ctx -> (Act_Pending "SPACE"));
        mode = Mode_Jmp;
    };
    {
        key = "ESC";
        prefix = None;
        action = (fun ctx -> (Act_Pending "ESC"));
        mode = Mode_Jmp;
    };

    (* EDT Mode *)
    {
        key = "SPACE";
        prefix = Some "ESC";
        action = (fun ctx -> (Act_ModeSwitch Mode_Jmp));
        mode = Mode_Edt;
    };
    {
        key = "i";
        prefix = Some "ESC";
        action = (fun ctx -> Act_ToggleStatus);
        mode = Mode_Edt;
    };
    {
        key = "w";
        prefix = Some "ESC";
        action = (fun ctx -> Act_MoveUp);
        mode = Mode_Edt;
    };
    {
        key = "s";
        prefix = Some "ESC";
        action = (fun ctx -> Act_MoveDown);
        mode = Mode_Edt;
    };
    {
        key = "a";
        prefix = Some "ESC";
        action = (fun ctx -> Act_MoveLeft);
        mode = Mode_Edt;
    };
    {
        key = "d";
        prefix = Some "ESC";
        action = (fun ctx -> Act_MoveRight);
        mode = Mode_Edt;
    };
    {
        key = "ESC";
        prefix = None;
        action = (fun ctx -> (Act_Pending "ESC"));
        mode = Mode_Edt;
    }
]
