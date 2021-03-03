let
  ps1 = builtins.fetchGit {
    url = "https://github.com/jonmosco/kube-ps1.git";
    ref = "v0.7.0";
    rev = "9efc67f9d3c27119a7abe9505f2262289d240f94";
  };
in
{
  files = {
    "per-user/kube-ps1.sh".text = builtins.readFile "${ps1}/kube-ps1.sh";
  };
}
