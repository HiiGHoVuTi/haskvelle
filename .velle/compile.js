
/*
  [ "stack build"
  , "cp .stack-work/dist/*\/*\/build/haskvelle-exe/haskvelle-exe ./haskvelle"
  , "python ./print-size.py"
  , "mv ./haskvelle ~/.velle/velle"
  ]
*/

const EXE_PATH = "./result/bin/velle"

function main() {
    function check (cmd, msg) {
        if (!cmd) return !log(msg);
        return true;
    }

    check(
        exec("nix-build release.nix"),
        /* OR */ "Couldn't build."
    );

    check(
        cpfile(EXE_PATH, "/home/maxime/.velle/velle"),
        /* OR */ "Couldn't copy the executable file to PATH."
    );

    rmdir("./result");
}

main();
