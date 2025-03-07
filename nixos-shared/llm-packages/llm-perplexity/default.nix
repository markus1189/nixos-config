{
  lib,
  python3,
  openai,
  source
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-perplexity";
  version = "0.42.42";
  pyproject = true;

  src = source;

  build-system = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  dependencies = [
    python3.pkgs.openai
  ];

  pythonImportsCheck = [
    # "llm_perplexity"
  ];

  dontCheckRuntimeDeps = true;

  meta = {
    description = "LLM access to pplx-api";
    homepage = "https://github.com/hex/llm-perplexity";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ ];
  };
}
