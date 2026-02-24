/**
 * Questionnaire Tool - Unified tool for asking single or multiple questions
 *
 * Single question: simple options list
 * Multiple questions: tab bar navigation between questions
 * Multi-select: Space/number to toggle, Enter to confirm
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Editor, type EditorTheme, Key, matchesKey, Text, truncateToWidth } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";

// Types
interface QuestionOption {
        value: string;
        label: string;
        description?: string;
}

type RenderOption = QuestionOption & { isOther?: boolean };

interface Question {
        id: string;
        label: string;
        prompt: string;
        options: QuestionOption[];
        allowOther: boolean;
        multiple?: boolean;
}

interface Answer {
        id: string;
        values: string[];   // machine values (array; one element for single-select)
        labels: string[];   // display labels (array; one element for single-select)
        wasCustom: boolean; // whether user typed at least one free-text answer
        indices?: number[]; // 1-based indices of predefined selected options
}

interface QuestionnaireResult {
        questions: Question[];
        answers: Answer[];
        cancelled: boolean;
}

// Schema
const QuestionOptionSchema = Type.Object({
        value: Type.String({ description: "The value returned when selected" }),
        label: Type.String({ description: "Display label for the option" }),
        description: Type.Optional(Type.String({ description: "Optional description shown below label" })),
});

const QuestionSchema = Type.Object({
        id: Type.String({ description: "Unique identifier for this question" }),
        label: Type.Optional(
                Type.String({
                        description: "Short contextual label for tab bar, e.g. 'Scope', 'Priority' (defaults to Q1, Q2)",
                }),
        ),
        prompt: Type.String({ description: "The full question text to display" }),
        options: Type.Array(QuestionOptionSchema, { description: "Available options to choose from" }),
        allowOther: Type.Optional(Type.Boolean({ description: "Allow 'Type something' option (always true, parameter ignored)" })),
        multiple: Type.Optional(Type.Boolean({ description: "Allow selecting multiple options. Use Space or number keys to toggle, Enter to confirm." })),
});

const QuestionnaireParams = Type.Object({
        questions: Type.Array(QuestionSchema, { description: "Questions to ask the user" }),
});

function errorResult(
        message: string,
        questions: Question[] = [],
): { content: { type: "text"; text: string }[]; details: QuestionnaireResult } {
        return {
                content: [{ type: "text", text: message }],
                details: { questions, answers: [], cancelled: true },
        };
}

export default function questionnaire(pi: ExtensionAPI) {
        pi.registerTool({
                name: "questionnaire",
                label: "Questionnaire",
                description:
                        "Use this tool when you need to ask the user questions before proceeding. This allows you to:\n" +
                        "1. Clarify ambiguous instructions or requirements\n" +
                        "2. Gather preferences or constraints before making decisions\n" +
                        "3. Offer choices at key implementation branch points\n" +
                        "4. Confirm direction before undertaking significant work\n" +
                        "\n" +
                        "Usage notes:\n" +
                        "- A 'Type something' option is always added automatically — do not include 'Other', 'Something else', or any catch-all option\n" +
                        "- Set multiple: true to allow selecting more than one answer\n" +
                        "- If you recommend a specific option, make it first in the list and append '(Recommended)' to its label\n" +
                        "- Keep option labels short and distinct; use the description field for elaboration\n" +
                        "- Use the label field for a concise tab name (e.g. 'Scope', 'Timeline') — defaults to Q1, Q2…",
                parameters: QuestionnaireParams,

                async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
                        if (!ctx.hasUI) {
                                return errorResult("Error: UI not available (running in non-interactive mode)");
                        }
                        if (params.questions.length === 0) {
                                return errorResult("Error: No questions provided");
                        }

                        // Normalize questions with defaults - always allow typing
                        const questions: Question[] = params.questions.map((q, i) => ({
                                ...q,
                                label: q.label || `Q${i + 1}`,
                                allowOther: true, // Always enforce typing option
                        }));

                        const isMulti = questions.length > 1;
                        const totalTabs = questions.length + 1; // questions + Submit

                        const result = await ctx.ui.custom<QuestionnaireResult>((tui, theme, _kb, done) => {
                                // State
                                let currentTab = 0;
                                let optionIndex = 0;
                                let inputMode = false;
                                let inputQuestionId: string | null = null;
                                let cachedLines: string[] | undefined;
                                const answers = new Map<string, Answer>();

                                // Multi-select state: question id → { indices: Set of 0-based option indices, custom?: string }
                                const multiSelections = new Map<string, { indices: Set<number>; custom?: string }>();

                                // Editor for "Type something" option
                                const editorTheme: EditorTheme = {
                                        borderColor: (s) => theme.fg("accent", s),
                                        selectList: {
                                                selectedPrefix: (t) => theme.fg("accent", t),
                                                selectedText: (t) => theme.fg("accent", t),
                                                description: (t) => theme.fg("muted", t),
                                                scrollInfo: (t) => theme.fg("dim", t),
                                                noMatch: (t) => theme.fg("warning", t),
                                        },
                                };
                                const editor = new Editor(tui, editorTheme);

                                // Helpers
                                function refresh() {
                                        cachedLines = undefined;
                                        tui.requestRender();
                                }

                                function submit(cancelled: boolean) {
                                        done({ questions, answers: Array.from(answers.values()), cancelled });
                                }

                                function currentQuestion(): Question | undefined {
                                        return questions[currentTab];
                                }

                                function currentOptions(): RenderOption[] {
                                        const q = currentQuestion();
                                        if (!q) return [];
                                        const opts: RenderOption[] = [...q.options];
                                        if (q.allowOther) {
                                                opts.push({ value: "__other__", label: "Type something.", isOther: true });
                                        }
                                        return opts;
                                }

                                function allAnswered(): boolean {
                                        return questions.every((q) => answers.has(q.id));
                                }

                                function advanceAfterAnswer() {
                                        if (!isMulti) {
                                                submit(false);
                                                return;
                                        }
                                        if (currentTab < questions.length - 1) {
                                                currentTab++;
                                        } else {
                                                currentTab = questions.length; // Submit tab
                                        }
                                        optionIndex = 0;
                                        refresh();
                                }

                                // Single-select: save as single-element arrays
                                function saveAnswer(questionId: string, value: string, label: string, wasCustom: boolean, index?: number) {
                                        answers.set(questionId, {
                                                id: questionId,
                                                values: [value],
                                                labels: [label],
                                                wasCustom,
                                                indices: index !== undefined ? [index] : undefined,
                                        });
                                }

                                // Multi-select helpers
                                function getMultiSelection(questionId: string): { indices: Set<number>; custom?: string } {
                                        if (!multiSelections.has(questionId)) {
                                                multiSelections.set(questionId, { indices: new Set() });
                                        }
                                        return multiSelections.get(questionId)!;
                                }

                                function toggleMultiOption(questionId: string, optIndex: number) {
                                        const sel = getMultiSelection(questionId);
                                        if (sel.indices.has(optIndex)) {
                                                sel.indices.delete(optIndex);
                                        } else {
                                                sel.indices.add(optIndex);
                                        }
                                }

                                function hasAnyMultiSelection(questionId: string): boolean {
                                        const sel = multiSelections.get(questionId);
                                        if (!sel) return false;
                                        return sel.indices.size > 0 || sel.custom !== undefined;
                                }

                                function saveMultiAnswer(questionId: string, opts: RenderOption[]) {
                                        const sel = getMultiSelection(questionId);
                                        const sortedIndices = Array.from(sel.indices).sort((a, b) => a - b);
                                        const selectedOpts = sortedIndices.map((i) => opts[i]).filter(Boolean);
                                        const values: string[] = selectedOpts.map((o) => o.value);
                                        const labels: string[] = selectedOpts.map((o) => o.label);
                                        const indices: number[] = sortedIndices.map((i) => i + 1); // 1-based

                                        if (sel.custom !== undefined) {
                                                values.push(sel.custom);
                                                labels.push(sel.custom);
                                        }

                                        answers.set(questionId, {
                                                id: questionId,
                                                values,
                                                labels,
                                                wasCustom: sel.custom !== undefined,
                                                indices,
                                        });
                                }

                                // Editor submit callback
                                editor.onSubmit = (value) => {
                                        if (!inputQuestionId) return;
                                        const trimmed = value.trim() || "(no response)";
                                        const q = questions.find((q) => q.id === inputQuestionId);

                                        if (q?.multiple) {
                                                // Multi-select: store custom text, stay on question so user can confirm
                                                const sel = getMultiSelection(inputQuestionId);
                                                sel.custom = trimmed;
                                                inputMode = false;
                                                inputQuestionId = null;
                                                editor.setText("");
                                                refresh();
                                        } else {
                                                // Single-select: save and advance
                                                saveAnswer(inputQuestionId, trimmed, trimmed, true);
                                                inputMode = false;
                                                inputQuestionId = null;
                                                editor.setText("");
                                                advanceAfterAnswer();
                                        }
                                };

                                function handleInput(data: string) {
                                        // Input mode: route to editor
                                        if (inputMode) {
                                                if (matchesKey(data, Key.escape)) {
                                                        inputMode = false;
                                                        inputQuestionId = null;
                                                        editor.setText("");
                                                        refresh();
                                                        return;
                                                }
                                                editor.handleInput(data);
                                                refresh();
                                                return;
                                        }

                                        const q = currentQuestion();
                                        const opts = currentOptions();

                                        // Tab navigation (multi-question only)
                                        if (isMulti) {
                                                if (matchesKey(data, Key.tab) || matchesKey(data, Key.right)) {
                                                        currentTab = (currentTab + 1) % totalTabs;
                                                        optionIndex = 0;
                                                        refresh();
                                                        return;
                                                }
                                                if (matchesKey(data, Key.shift("tab")) || matchesKey(data, Key.left)) {
                                                        currentTab = (currentTab - 1 + totalTabs) % totalTabs;
                                                        optionIndex = 0;
                                                        refresh();
                                                        return;
                                                }
                                        }

                                        // Submit tab
                                        if (currentTab === questions.length) {
                                                if (matchesKey(data, Key.enter) && allAnswered()) {
                                                        submit(false);
                                                } else if (matchesKey(data, Key.escape)) {
                                                        submit(true);
                                                } else if (data.toLowerCase() === "a" && allAnswered()) {
                                                        // "Anything else?" option
                                                        inputMode = true;
                                                        inputQuestionId = "__additional__";
                                                        editor.setText(answers.get("__additional__")?.labels[0] || "");
                                                        refresh();
                                                        return;
                                                }
                                                return;
                                        }

                                        // Option navigation (shared)
                                        if (matchesKey(data, Key.up)) {
                                                optionIndex = Math.max(0, optionIndex - 1);
                                                refresh();
                                                return;
                                        }
                                        if (matchesKey(data, Key.down)) {
                                                optionIndex = Math.min(opts.length - 1, optionIndex + 1);
                                                refresh();
                                                return;
                                        }

                                        if (q?.multiple) {
                                                // ── Multi-select mode ──
                                                const num = parseInt(data, 10);
                                                const isNumKey = num >= 1 && num <= opts.length;
                                                const isSpace = data === " ";
                                                const isEnter = matchesKey(data, Key.enter);

                                                // Number keys move cursor AND toggle
                                                if (isNumKey) {
                                                        optionIndex = num - 1;
                                                }

                                                const activeOpt = opts[optionIndex];

                                                if (isNumKey || isSpace) {
                                                        if (activeOpt?.isOther) {
                                                                // Open editor for custom text (pre-fill if already typed)
                                                                const sel = getMultiSelection(q.id);
                                                                inputMode = true;
                                                                inputQuestionId = q.id;
                                                                editor.setText(sel.custom || "");
                                                        } else {
                                                                toggleMultiOption(q.id, optionIndex);
                                                        }
                                                        refresh();
                                                        return;
                                                }

                                                if (isEnter) {
                                                        if (activeOpt?.isOther) {
                                                                // Enter on "Type something": open editor
                                                                const sel = getMultiSelection(q.id);
                                                                inputMode = true;
                                                                inputQuestionId = q.id;
                                                                editor.setText(sel.custom || "");
                                                                refresh();
                                                                return;
                                                        }
                                                        if (hasAnyMultiSelection(q.id)) {
                                                                saveMultiAnswer(q.id, opts);
                                                                advanceAfterAnswer();
                                                        }
                                                        // Nothing selected → do nothing (render shows hint)
                                                        return;
                                                }
                                        } else {
                                                // ── Single-select mode ──
                                                // Number key selection (1-9)
                                                if (q) {
                                                        const num = parseInt(data, 10);
                                                        if (num >= 1 && num <= opts.length) {
                                                                optionIndex = num - 1;
                                                                const opt = opts[optionIndex];
                                                                if (opt.isOther) {
                                                                        inputMode = true;
                                                                        inputQuestionId = q.id;
                                                                        editor.setText("");
                                                                } else {
                                                                        saveAnswer(q.id, opt.value, opt.label, false, num);
                                                                        advanceAfterAnswer();
                                                                }
                                                                refresh();
                                                                return;
                                                        }
                                                }

                                                // Enter to select
                                                if (matchesKey(data, Key.enter) && q) {
                                                        const opt = opts[optionIndex];
                                                        if (opt.isOther) {
                                                                inputMode = true;
                                                                inputQuestionId = q.id;
                                                                editor.setText("");
                                                                refresh();
                                                                return;
                                                        }
                                                        saveAnswer(q.id, opt.value, opt.label, false, optionIndex + 1);
                                                        advanceAfterAnswer();
                                                        return;
                                                }
                                        }

                                        // Cancel
                                        if (matchesKey(data, Key.escape)) {
                                                submit(true);
                                        }
                                }

                                function render(width: number): string[] {
                                        if (cachedLines) return cachedLines;

                                        const lines: string[] = [];
                                        const q = currentQuestion();
                                        const opts = currentOptions();

                                        // Helper to add truncated line
                                        const add = (s: string) => lines.push(truncateToWidth(s, width));

                                        add(theme.fg("accent", "─".repeat(width)));

                                        // Tab bar (multi-question only)
                                        if (isMulti) {
                                                const tabs: string[] = ["← "];
                                                for (let i = 0; i < questions.length; i++) {
                                                        const isActive = i === currentTab;
                                                        const isAnswered = answers.has(questions[i].id);
                                                        const lbl = questions[i].label;
                                                        const box = isAnswered ? "■" : "□";
                                                        const color = isAnswered ? "success" : "muted";
                                                        const text = ` ${box} ${lbl} `;
                                                        const styled = isActive ? theme.bg("selectedBg", theme.fg("text", text)) : theme.fg(color, text);
                                                        tabs.push(`${styled} `);
                                                }
                                                const canSubmit = allAnswered();
                                                const isSubmitTab = currentTab === questions.length;
                                                const submitText = " ✓ Submit ";
                                                const submitStyled = isSubmitTab
                                                        ? theme.bg("selectedBg", theme.fg("text", submitText))
                                                        : theme.fg(canSubmit ? "success" : "dim", submitText);
                                                tabs.push(`${submitStyled} →`);
                                                add(` ${tabs.join("")}`);
                                                lines.push("");
                                        }

                                        // Render options list (used in normal and input-mode views)
                                        function renderOptions() {
                                                const isMultiple = q?.multiple === true;
                                                const multiSel = (q && isMultiple) ? getMultiSelection(q.id) : null;

                                                for (let i = 0; i < opts.length; i++) {
                                                        const opt = opts[i];
                                                        const cursorOn = i === optionIndex;
                                                        const isOther = opt.isOther === true;
                                                        const cursorPrefix = cursorOn ? theme.fg("accent", "> ") : "  ";
                                                        const color = cursorOn ? "accent" : "text";

                                                        if (isMultiple && multiSel) {
                                                                const isChecked = isOther
                                                                        ? multiSel.custom !== undefined
                                                                        : multiSel.indices.has(i);
                                                                const checkbox = isChecked
                                                                        ? theme.fg("success", "[x]")
                                                                        : theme.fg("muted", "[ ]");

                                                                if (isOther && inputMode && inputQuestionId === q!.id) {
                                                                        add(cursorPrefix + checkbox + " " + theme.fg("accent", `${i + 1}. ${opt.label} ✎`));
                                                                } else if (isOther && multiSel.custom !== undefined) {
                                                                        // Show the already-typed text inline
                                                                        add(cursorPrefix + checkbox + " " + theme.fg(color, `${i + 1}. Type something`) + theme.fg("muted", `: "${multiSel.custom}"`));
                                                                } else {
                                                                        add(cursorPrefix + checkbox + " " + theme.fg(color, `${i + 1}. ${opt.label}`));
                                                                }
                                                        } else {
                                                                // Single-select: cursor indicator only
                                                                if (isOther && inputMode) {
                                                                        add(cursorPrefix + theme.fg("accent", `${i + 1}. ${opt.label} ✎`));
                                                                } else {
                                                                        add(cursorPrefix + theme.fg(color, `${i + 1}. ${opt.label}`));
                                                                }
                                                        }

                                                        if (opt.description) {
                                                                add(`     ${theme.fg("muted", opt.description)}`);
                                                        }
                                                }
                                        }

                                        // Content
                                        if (inputMode && q) {
                                                add(theme.fg("text", ` ${q.prompt}`));
                                                lines.push("");
                                                // Show options for reference (with current checkboxes)
                                                renderOptions();
                                                lines.push("");
                                                add(theme.fg("muted", " Your answer:"));
                                                for (const line of editor.render(width - 2)) {
                                                        add(` ${line}`);
                                                }
                                                lines.push("");
                                                add(theme.fg("dim", " Enter to submit • Esc to cancel"));
                                        } else if (currentTab === questions.length) {
                                                add(theme.fg("accent", theme.bold(" Ready to submit")));
                                                lines.push("");
                                                for (const question of questions) {
                                                        const answer = answers.get(question.id);
                                                        if (answer) {
                                                                let labelText: string;
                                                                if (answer.wasCustom && answer.labels.length === 1) {
                                                                        labelText = `(wrote) ${answer.labels[0]}`;
                                                                } else {
                                                                        const predefined = answer.wasCustom ? answer.labels.slice(0, -1) : answer.labels;
                                                                        const parts = [...predefined];
                                                                        if (answer.wasCustom) {
                                                                                parts.push(`"${answer.labels[answer.labels.length - 1]}"`);
                                                                        }
                                                                        labelText = parts.join(", ");
                                                                }
                                                                add(`${theme.fg("muted", ` ${question.label}: `)}${theme.fg("text", labelText)}`);
                                                        }
                                                }
                                                lines.push("");

                                                // "Anything else?" option
                                                const additionalAnswer = answers.get("__additional__");
                                                if (additionalAnswer) {
                                                        add(theme.fg("muted", " Anything else: ") + theme.fg("text", additionalAnswer.labels[0]));
                                                        lines.push("");
                                                }

                                                if (allAnswered()) {
                                                        add(theme.fg("success", " Press Enter to submit"));
                                                        add(theme.fg("dim", " Press A to add 'Anything else'"));
                                                } else {
                                                        const missing = questions
                                                                .filter((q) => !answers.has(q.id))
                                                                .map((q) => q.label)
                                                                .join(", ");
                                                        add(theme.fg("warning", ` Unanswered: ${missing}`));
                                                }
                                        } else if (q) {
                                                add(theme.fg("text", ` ${q.prompt}`));
                                                lines.push("");
                                                renderOptions();

                                                // Multi-select: show contextual hint below options
                                                if (q.multiple) {
                                                        lines.push("");
                                                        if (hasAnyMultiSelection(q.id)) {
                                                                add(theme.fg("success", " Press Enter to confirm selections"));
                                                        } else {
                                                                add(theme.fg("dim", " Select at least one option, then press Enter"));
                                                        }
                                                }
                                        }

                                        lines.push("");
                                        if (!inputMode) {
                                                let help: string;
                                                if (q?.multiple) {
                                                        help = isMulti
                                                                ? " Tab/←→ navigate • ↑↓ move • 1-9/Space toggle • Enter confirm • Esc cancel"
                                                                : " ↑↓ move • 1-9/Space toggle • Enter confirm • Esc cancel";
                                                } else {
                                                        help = isMulti
                                                                ? " Tab/←→ navigate • ↑↓/1-9 select • Enter confirm • Esc cancel"
                                                                : " ↑↓/1-9 select • Enter confirm • Esc cancel";
                                                }
                                                add(theme.fg("dim", help));
                                        }
                                        add(theme.fg("accent", "─".repeat(width)));

                                        cachedLines = lines;
                                        return lines;
                                }

                                return {
                                        render,
                                        invalidate: () => {
                                                cachedLines = undefined;
                                        },
                                        handleInput,
                                };
                        });

                        if (result.cancelled) {
                                return {
                                        content: [{ type: "text", text: "User cancelled the questionnaire" }],
                                        details: result,
                                };
                        }

                        const answerLines = result.answers.map((a) => {
                                if (a.id === "__additional__") {
                                        return `Additional notes: ${a.labels[0]}`;
                                }

                                const qLabel = questions.find((q) => q.id === a.id)?.label || a.id;
                                const parts: string[] = [];

                                // Predefined selections (all labels except last if wasCustom)
                                const predefinedLabels = a.wasCustom ? a.labels.slice(0, -1) : a.labels;
                                const predefinedIndices = a.indices || [];
                                if (predefinedLabels.length > 0) {
                                        const selections = predefinedLabels
                                                .map((label, i) =>
                                                        predefinedIndices[i] !== undefined ? `${predefinedIndices[i]}. ${label}` : label,
                                                )
                                                .join(", ");
                                        parts.push(`user selected: ${selections}`);
                                }

                                if (a.wasCustom) {
                                        parts.push(`user wrote: ${a.labels[a.labels.length - 1]}`);
                                }

                                return `${qLabel}: ${parts.join(" and ")}`;
                        });

                        return {
                                content: [{ type: "text", text: answerLines.join("\n") }],
                                details: result,
                        };
                },

                renderCall(args, theme) {
                        const qs = (args.questions as Question[]) || [];
                        const count = qs.length;
                        const labels = qs.map((q) => q.label || q.id).join(", ");
                        let text = theme.fg("toolTitle", theme.bold("questionnaire "));
                        text += theme.fg("muted", `${count} question${count !== 1 ? "s" : ""}`);
                        if (labels) {
                                text += theme.fg("dim", ` (${truncateToWidth(labels, 40)})`);
                        }
                        return new Text(text, 0, 0);
                },

                renderResult(result, _options, theme) {
                        const details = result.details as QuestionnaireResult | undefined;
                        if (!details) {
                                const text = result.content[0];
                                return new Text(text?.type === "text" ? text.text : "", 0, 0);
                        }
                        if (details.cancelled) {
                                return new Text(theme.fg("warning", "Cancelled"), 0, 0);
                        }
                        const lines = details.answers.map((a) => {
                                if (a.id === "__additional__") {
                                        return `${theme.fg("success", "✓ ")}${theme.fg("muted", "Additional: ")}${a.labels[0]}`;
                                }
                                if (a.wasCustom && a.labels.length === 1) {
                                        return `${theme.fg("success", "✓ ")}${theme.fg("accent", a.id)}: ${theme.fg("muted", "(wrote) ")}${a.labels[0]}`;
                                }
                                const predefinedLabels = a.wasCustom ? a.labels.slice(0, -1) : a.labels;
                                const predefinedIndices = a.indices || [];
                                const displayParts: string[] = predefinedLabels.map((label, i) => {
                                        const idx = predefinedIndices[i];
                                        return idx !== undefined ? `${idx}. ${label}` : label;
                                });
                                if (a.wasCustom) {
                                        displayParts.push(`"${a.labels[a.labels.length - 1]}"`);
                                }
                                return `${theme.fg("success", "✓ ")}${theme.fg("accent", a.id)}: ${displayParts.join(", ")}`;
                        });
                        return new Text(lines.join("\n"), 0, 0);
                },
        });
}
