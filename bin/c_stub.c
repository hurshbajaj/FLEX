#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <string.h>

extern char* highlight_ocaml(const unsigned char* src, size_t len, void* cfg, void* theme);
extern void* get_lang_config(void* theme);
extern void free_rust_string(char* s);
extern char* get_ui_colors(void* theme, const char* key);
extern void* get_theme(const unsigned char* src, size_t len);

static struct custom_operations config_ops = {
    "rust.config",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
};

static struct custom_operations theme_ops = {
    "rust.theme",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
};

CAMLprim value ocaml_get_lang_config(value theme_val) {
    CAMLparam1(theme_val);
    CAMLlocal1(v);
    
    void* theme = *((void**)Data_custom_val(theme_val));
    
    v = caml_alloc_custom(&config_ops, sizeof(void*), 0, 1);
    *((void**)Data_custom_val(v)) = get_lang_config(theme);
    CAMLreturn(v);
}

CAMLprim value ocaml_get_theme(value src) {
    CAMLparam1(src);
    CAMLlocal1(v);
    
    const char* s = String_val(src);
    size_t len = caml_string_length(src);
    
    v = caml_alloc_custom(&theme_ops, sizeof(void*), 0, 1);
    *((void**)Data_custom_val(v)) = get_theme((const unsigned char*)s, len);
    CAMLreturn(v);
}

CAMLprim value ocaml_highlight(value src, value cfg, value theme_val) {
    CAMLparam3(src, cfg, theme_val);
    CAMLlocal1(out);
    
    const char* s = String_val(src);
    size_t len = caml_string_length(src);
    void* config = *((void**)Data_custom_val(cfg));
    void* theme = *((void**)Data_custom_val(theme_val));
    
    char* res = highlight_ocaml((const unsigned char*)s, len, config, theme);
    out = caml_copy_string(res);
    free_rust_string(res);
    
    CAMLreturn(out);
}

CAMLprim value ocaml_get_ui_colors(value theme_val, value key_val) {
    CAMLparam2(theme_val, key_val);
    CAMLlocal1(out);
    
    const char* key = String_val(key_val);
    void* theme = *((void**)Data_custom_val(theme_val));
    char* res = get_ui_colors(theme, key);
    out = caml_copy_string(res);
    free_rust_string(res);
    
    CAMLreturn(out);
}
