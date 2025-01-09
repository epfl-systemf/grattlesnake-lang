import shutil
import subprocess
import os
import sys
from typing import Final, Callable

version_number: Final[str] = "0.2.0-snapshot"

dst_dir: Final[str] = "../jars"

compiler_final_jar_name: Final[str] = "rattlesnake-compiler-" + version_number + ".jar"
runtime_final_jar_name: Final[str] = "rattlesnake-runtime-" + version_number + ".jar"
agent_final_jar_name: Final[str] = "rattlesnake-agent-" + version_number + ".jar"

compiler_dir: Final[str] = "../rattlesnake-compiler"
runtime_dir: Final[str] = "../rattlesnake-runtime"
agent_dir: Final[str] = "../rattlesnake-agent"

compiler_scala_version: Final[str] = "scala-3.5.0"


def assemble(dirpath: str):
    subprocess.Popen(["sbt", "assembly"], cwd=dirpath, shell=True, stdout=sys.stdout,
                     stderr=sys.stderr).wait()


def copyJar(src_dir: str, name_criterion: Callable[[str], bool], dst_file_name: str):
    for f in os.listdir(src_dir):
        if name_criterion(f):
            shutil.copy(src=src_dir + "/" + f, dst=dst_dir + "/" + dst_file_name)


def package(dirpath: str):
    subprocess.Popen(["mvn", "clean", "package"], cwd=dirpath, shell=True, stdout=sys.stdout,
                     stderr=sys.stderr).wait()


if __name__ == "__main__":
    assemble(compiler_dir)
    package(runtime_dir)
    package(agent_dir)
    shutil.rmtree(dst_dir, ignore_errors=True)
    os.mkdir(dst_dir)
    copyJar(compiler_dir + "/target/" + compiler_scala_version,
            lambda f: f.startswith("Rattlesnake-assembly"),
            compiler_final_jar_name)
    copyJar(runtime_dir + "/target/",
            lambda f: f.endswith("with-dependencies.jar"),
            runtime_final_jar_name)
    copyJar(agent_dir + "/target/",
            lambda f: f.endswith("with-dependencies.jar"),
            agent_final_jar_name)
