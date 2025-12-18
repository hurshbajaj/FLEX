#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <string.h>

extern void* get_lang_config(void);
extern char* highlight_ocaml(const unsigned char* src, size_t len, void* cfg);
extern void free_rust_string(char* s);

static struct custom_operations config_ops = {
    "rust.config",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
};

CAMLprim value ocaml_get_lang_config(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(v);

    v = caml_alloc_custom(&config_ops, sizeof(void*), 0, 1);
    *((void**)Data_custom_val(v)) = get_lang_config();

    CAMLreturn(v);
}

CAMLprim value ocaml_highlight(value src, value cfg) {
    CAMLparam2(src, cfg);
    CAMLlocal1(out);

    const char* s = String_val(src);
    size_t len = caml_string_length(src);
    void* config = *((void**)Data_custom_val(cfg));

    char* res = highlight_ocaml((const unsigned char*)s, len, config);
    out = caml_copy_string(res);
    free_rust_string(res);

    CAMLreturn(out);
}


