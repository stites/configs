{ pkgs }:

{
  shellAliases = {
    # git
    git   = "${pkgs.git-hub}/bin/hub";
    g     = "git ";
    gf    = "git fetch";
    gl    = "git log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%ad @ %cr) %C(bold blue)<%an>%Creset' --graph --abbrev-commit --date=short";
    go    = "git checkout ";
    god   = "git checkout develop && git pull upstream develop";
    gs    = "git status";
    gr    = "git reset";
    gm    = "git merge ";
    ga    = "git add ";
    "ga." = "git add . ";
    gd    = "git diff";
    gb    = "git branch";
    gmv   = "git mv ";
    grm   = "git rm ";

    gc    = "git commit ";
    gcm   = "git commit -m ";
    gca   = "git commit --amend ";
    gcma  = "git commit --amend -m ";

    gp    = "git push";
    gpo   = "git push origin";
    gpom  = "git push origin   master";
    gpod  = "git push origin   develop";
    gpocb = "git push origin   $(cb)";
    gpu   = "git push upstream";
    gpum  = "git push upstream master";
    gpud  = "git push upstream develop";
    gpucb = "git push upstream $(cb)";

    gpl    = "git pull";
    gplo   = "git pull origin";
    gplom  = "git pull origin master";
    gplod  = "git pull origin develop";
    gplocb = "git pull origin $(cb)";
    gplu   = "git pull upstream";
    gplum  = "git pull upstream master";
    gplud  = "git pull upstream develop";
    gplucb = "git pull upstream $(cb)";

    stash = "git stash";
    pop   = "git stash pop";

    # hub-functionality
    gpr   = "${pkgs.git-hub}/bin/hub pull-request";
    gprud = "${pkgs.git-hub}/bin/hub pull-request -b $(remote-name upstream)\\/$(repo-name upstream):develop -h $(remote-name origin)\\/$(repo-name origin):$(cb)";
  };

  functions = ''
    function cb {
      ${pkgs.git}/bin/git status | grep "On branch " | cut -c 11-
    }
    function remote-full {
      ${pkgs.git}/bin/git remote -v | grep "$1" | head -n 1 | cut -d ':' -f 2 | cut -d '.' -f 1
    }
    function remote-name {
      remote-full "$1" | cut -d '/' -f 1
    }
    function repo-name {
      remote-full "$1" | cut -d '/' -f 2
    }
  '';
}
