
// Velle uses JavaScript for its..well, scripts.
// They can be found in a project's .velle folder, and will be executed on events, or with the `Velle > run` command.
// They use a small subset of javascript, but some functions are added.

const x = 12;
print(12); // print is a debugging function that will pretty-print anything given to it, and return the result

log("Welcome to the circus !"); // Log will log a single string

log("Hello " + print(x)); // Number 12.0
                          // Hello 12

// Here are instructions to interact with the outside world:
exec("echo hello");

log("Github username: " + readUserConfig("github.username"));
log("Project name: "    + readLocalConfig("project.name"));
