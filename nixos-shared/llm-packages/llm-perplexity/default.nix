{
  lib,
  python3,
  fetchFromGitHub,
  openai
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-perplexity";
  version = "2025.1.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "hex";
    repo = "llm-perplexity";
    rev = version;
    hash = "sha256-OweM9Pf2RqIbmRfz9oZuTmu0lfEIQQJxgFcoU6GzKas=";
  };

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
