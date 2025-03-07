{ lib
, python3
, source
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-gemini";
  version = "0.9";
  pyproject = true;

  src = source;

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    httpx
    ijson
    llm
  ];

  passthru.optional-dependencies = with python3.pkgs; {
    test = [
      pytest
    ];
  };

  pythonImportsCheck = [ ];

  dontCheckRuntimeDeps = true;

  meta = with lib; {
    description = "LLM plugin to access Google's Gemini family of models";
    homepage = "https://github.com/simonw/llm-gemini";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
    mainProgram = "llm-gemini";
  };
}
