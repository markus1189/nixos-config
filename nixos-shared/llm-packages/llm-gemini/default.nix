{ lib
, python3
, fetchFromGitHub
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-gemini";
  version = "0.9";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "simonw";
    repo = "llm-gemini";
    rev = version;
    hash = "sha256-XsQJByWZ9obwqlSmKZJx+RHWC/zxPYmjAsWx9Eh95gc=";
  };

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    httpx
    ijson
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
