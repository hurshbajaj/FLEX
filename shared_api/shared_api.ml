type mode = Mode_Edt | Mode_Jmp

type action = 
    | Act_MoveUp | Act_MoveDown | Act_MoveLeft | Act_MoveRight 
    | Act_PageDown | Act_PageUp | Act_EoL | Act_BoL
    | Act_Quit 
    | Act_RepLast
    | Act_NONE 
    | Act_Seq of action list
    | Act_SeqUndo of action list * bool
    | Act_ModeSwitch of mode
    | Act_I_AddStr of string * (int * int) | Act_I_RmChar | Act_I_InsertLine

    | Act_StatusI | Act_ToggleStatus

    | Act_VpShiftX | Act_XVpShiftX

    | Act_Pending of string
    | Act_Undo
    | Act_KillLine of int | Act_KillLineShift of int | Act_InsertLine of int * string
    | Act_CenterLine of int

    | Act_ToBufferTop | Act_ToBufferBottom

    | Act_AddCharStr of int * int * bool * string (* line, start idx, W.E. , content *)
    | Act_RmCharStr of int * int * int * bool (* line, start idx, length, W.E. *)

type keymap = {
    key:string;
    prefix: string option;
    action: action list;
    mode: mode;
}

type api_context = {
    current_line: int
}

