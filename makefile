REPO_ROOT := $(CURDIR)

all:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "    test-humor   Run humor override unit tests"
	@echo "    preflight    Run repo structural checks"

# Run humor unit tests
test-humor:
	sbcl --load src/ai/advanced-reasoning-core/logic-exe.lisp \
	     --load src/ai/advanced-reasoning-core/humor-classifier.lisp \
	     --load src/ai/advanced-reasoning-core/humor_injection_ai_override.lisp \
	     --load tests/test_humor_override.lisp \
	     --eval "(fiveam:run! 'ai.advanced-reasoning-core.tests.humor-override:humor-override-suite)" \
	     --quit

# Run preflight PowerShell hygiene checks (cross-platform if pwsh is installed)
preflight:
	pwsh ./scripts/humor-bot-ai.ps1
