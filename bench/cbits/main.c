#include <stdarg.h>
#include <stdlib.h>
#include "HsFFI.h"
#include "Rts.h"

extern StgClosure ZCMain_main_closure;

static void envRtsMsgFunction(const char *s, va_list ap) {
    const char *trace = getenv("TRACE");
    if (trace == NULL || trace[0] != '0') {
        rtsDebugMsgFn(s, ap);
    }
}

int main(int argc, char *argv[]) {
    RtsConfig rtsConfig = defaultRtsConfig;
    rtsConfig.rts_opts = "-N -qg";
    debugMsgFn = envRtsMsgFunction;
    hs_main(argc, argv, &ZCMain_main_closure, rtsConfig);
}
