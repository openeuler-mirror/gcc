/* Check the property contract observed by consumers after early loop work.  */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "function.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "context.h"

int plugin_is_GPL_compatible;

namespace {

unsigned checks;

const pass_data property_data =
{
  GIMPLE_PASS, "immutable-property-check", OPTGROUP_NONE, TV_NONE,
  PROP_cfg | PROP_ssa, 0, 0, 0, 0
};

class property_check : public gimple_opt_pass
{
public:
  property_check (gcc::context *ctxt)
    : gimple_opt_pass (property_data, ctxt)
  {}

  opt_pass *clone () override
  {
    return new property_check (m_ctxt);
  }

  unsigned execute (function *fun) override
  {
    ++checks;
    if (fun->curr_properties & PROP_loop_opts_done)
      error ("early immutable loop stage completed all loop optimizations");
    return 0;
  }
};

void
finish (void *, void *)
{
  if (!checks)
    error ("immutable loop stages were not exercised");
}

} // anonymous namespace

int
plugin_init (struct plugin_name_args *info,
	     struct plugin_gcc_version *version)
{
  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  register_pass_info pass;
  pass.pass = new property_check (g);
  pass.reference_pass_name = "immutable-done";
  pass.ref_pass_instance_number = 0;
  pass.pos_op = PASS_POS_INSERT_AFTER;
  register_callback (info->base_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass);
  register_callback (info->base_name, PLUGIN_FINISH, finish, NULL);
  return 0;
}
