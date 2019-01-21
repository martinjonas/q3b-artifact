import os

import benchexec.util as util
import benchexec.tools.template
import benchexec.result as result

REQUIRED_PATHS = []

class Tool(benchexec.tools.template.BaseTool):
    def executable(self):
        return util.find_executable('/home/cav/artifact/solvers/Boolector/boolector-3.0.0/build/bin/boolector')

    def version(self, executable):
        return self._version_from_tool(executable)

    def name(self):
        return 'boolector'
    
    def cmdline(self, executable, options, tasks, propertyfile, rlimits):
        assert len(tasks) == 1

        return [executable] + tasks + options

    def determine_result(self, returncode, returnsignal, output, isTimeout):
        if isTimeout:
            return 'timeout'

        for line in output:
            if line.startswith('sat'):
                return result.RESULT_SAT
            elif line.startswith('unsat'):
                return result.RESULT_UNSAT
        return result.RESULT_ERROR
