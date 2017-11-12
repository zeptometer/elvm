#include <ir/ir.h>
#include <target/util.h>
#include <target/scm_sr_lib.h>

static void scm_sr_emit_file_prologue(void) {
  emit_line(scm_sr_lib);
}

static void scm_sr_emit_func_prologue(int i) {
  emit_line("(define-syntax func-impl%d!", i);
  inc_indent();
  emit_line("(syntax-rules (quote) ((_) (ck s '(");
  inc_indent();
}

static void scm_sr_emit_func_epilogue() {
  dec_indent();
  emit_line(")))))");
  dec_indent();
}

static char* scm_sr_op_str(Inst* inst) {
  switch (inst->op) {
  case MOV:
    return "\"MOV\"";
  case ADD:
    return "\"ADD\"";
  case SUB:
    return "\"SUB\"";
  case LOAD:
    return "\"LOAD\"";
  case STORE:
    return "\"STORE\"";
  case PUTC:
    return "\"PUTC\"";
  case EQ:
  case JEQ:
    return "\"EQ\"";
  case NE:
  case JNE:
    return "\"NE\"";
  case LT:
  case JLT:
    return "\"LT\"";
  case GT:
  case JGT:
    return "\"GT\"";
  case LE:
  case JLE:
    return "\"LE\"";
  case GE:
  case JGE:
    return "\"GE\"";
  default:
    error("oopsa %d", inst->op);
  }
  return "BAD_INSTRUCTION";
}

static char* scm_sr_imm_str(int imm) {
  if (imm < 0) {
    return "POHE";
  } /* TO BE FIXED */

  if (imm == 0) {
    return "";
  } else {
    return format("%d %s", imm & 1, scm_sr_imm_str(imm >> 1));
  }
}

static char* scm_sr_src_str(Inst* inst) {
  if (inst->src.type == REG) {
    return format("(\"REG\" %s)", reg_names[inst->src.reg]);
  } else {
    return format("(\"IMM\" (%s))", scm_sr_imm_str(inst->src.imm));
  }
}

static void scm_sr_emit_inst(Inst* inst) {
  switch (inst->op) {
  case MOV:
  case ADD:
  case SUB:
  case LOAD:
  case STORE:
    emit_line("(%s %s %s)",
              scm_sr_op_str(inst),
	      reg_names[inst->dst.reg],
	      scm_sr_src_str(inst));
    break;

  case PUTC:
    emit_line("(\"PUTC\" %s)",
	      scm_sr_src_str(inst));
    break;

  case GETC:
    emit_line("(\"GETC\" %s)",
	      reg_names[inst->dst.reg]);
    break;

  case EXIT:
    emit_line("(\"EXIT\")");
    return;

  case DUMP:
    break;

  case EQ:
  case NE:
  case LT:
  case GT:
  case LE:
  case GE:
    emit_line("(\"CMP\" %s %s %s)",
              scm_sr_op_str(inst),
	      reg_names[inst->dst.reg],
	      scm_sr_src_str(inst));
    break;

  case JEQ:
  case JNE:
  case JLT:
  case JGT:
  case JLE:
  case JGE:
    emit_line("(\"JCOND\" %s %s %s)",
              scm_sr_op_str(inst),
	      reg_names[inst->dst.reg],
	      scm_sr_src_str(inst));
    break;

  case JMP:
    emit_line("(\"JMP\" %s)",
	      scm_sr_src_str(inst));
    break;

  default:
    error("oops");
  }
  return;
}

static void scm_sr_emit_func_impl(Inst* inst) {
  int prev_pc = 0;
  scm_sr_emit_func_prologue(0);
  for (; inst; inst = inst->next) {
    if (prev_pc != inst->pc) {
      scm_sr_emit_func_epilogue();
      emit_line("");
      scm_sr_emit_func_prologue(inst->pc);
    }
    prev_pc = inst->pc;

    scm_sr_emit_inst(inst);
  }
  scm_sr_emit_func_epilogue();
}

static int scm_sc_count_pc(Inst* inst) {
  int max_pc = 0;
  for (; inst; inst = inst->next) {
    max_pc = (max_pc > inst->pc)?max_pc:inst->pc;
  }
  return max_pc;
}

static void scm_sr_emit_inst_mem(int max_pc) {
  (void)max_pc;
  /* fixme */
}

static void scm_sr_emit_data_mem(Data* data) {
  (void)data;
  /* fixme */
}

static const char* SCM_SR_REG_NAMES[6] = {
  "\"A\"", "\"B\"", "\"C\"", "\"D\"", "\"BP\"", "\"SP\""
};

void target_scm_sr(Module* module) {
  reg_names = SCM_SR_REG_NAMES;

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
