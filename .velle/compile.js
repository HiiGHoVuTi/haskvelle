
/*
  [ "stack build"
  , "cp .stack-work/dist/*\/*\/build/haskvelle-exe/haskvelle-exe ./haskvelle"
  , "python ./print-size.py"
  , "mv ./haskvelle ~/.velle/velle"
  ]
*/

const EXE_PATH = ".stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/haskvelle-exe/haskvelle-exe";

function main() {
    function check (cmd, msg) {
        if (!cmd) return !log(msg);
        return true;
    }

    check(
        exec("stack build"),
        /* OR */ "Couldn't build."
    );


    check(
        cpfile(EXE_PATH, "./haskvelle"),
        /* OR */ "Couldn't find executable file."
    );

    check(
        exec("strip -p ./haskvelle"),
        /* OR */ "Couldn't strip down the executable."
    );

    check(
        exec("python ./print-size.py"),
        /* OR */ "Failed to execute python file."
    );

    check(
        cpfile("./haskvelle", "/home/maxime/.velle/velle"),
        /* OR */ "Couldn't copy the executable file to PATH."
    );

    rmfile("./haskvelle");
}

main();
