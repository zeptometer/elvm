#include <ir/ir.h>
#include <target/util.h>
#include <target/scm_sr_lib.h>

static void scm_sr_emit_file_prologue(void) {
  emit_line(scm_sr_lib);
}

static void scm_sr_emit_func_prologue() {
  /* fixme */
}

static void scm_sr_emit_func_epilogue() {
  /* fixme */
}

static void scm_sr_emit_func_impl(Inst* inst) {
  (void) inst;
  scm_sr_emit_func_prologue();
  scm_sr_emit_func_epilogue();
  /* fixme */
}

static int scm_sc_count_pc(Inst* inst) {
  (void) inst;
  return 0;
  /* fixme */
}

static void scm_sr_emit_inst_mem(int max_pc) {
  (void)max_pc;
  /* fixme */
}

static void scm_sr_emit_data_mem(Data* data) {
  (void)data;
  /* fixme */
}

void target_scm_sr(Module* module) {
  scm_sr_emit_file_prologue();
  emit_line("");

  scm_sr_emit_func_impl(module->text);
  emit_line("");

  scm_sr_emit_inst_mem(scm_sc_count_pc(module->text));
  emit_line("");

  scm_sr_emit_data_mem(module->data);
  emit_line("");

  scm_sr_emit_file_prologue();
}
