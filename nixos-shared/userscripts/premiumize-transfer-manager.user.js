// ==UserScript==
// @name         Premiumize - Transfer Manager
// @namespace    https://www.premiumize.me/
// @version      2.0
// @description  Batch select & delete transfers with fuzzy matching and error filtering
// @match        https://www.premiumize.me/transfers*
// @grant        none
// ==/UserScript==

(function () {
  "use strict";

  const DELAY_MS = 300;
  const MODAL_ID = "pm-delete-modal";
  const TOOLBAR_ID = "pm-toolbar";

  // ── Helpers ──

  function sleep(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  async function fetchTransfers() {
    const resp = await fetch("/api/transfer/list");
    const data = await resp.json();
    return data.transfers || [];
  }

  async function deleteTransfer(id) {
    const form = new FormData();
    form.append("id", id);
    const resp = await fetch("/api/transfer/delete", {
      method: "POST",
      body: form,
    });
    return resp.json();
  }

  function tokenMatch(query, text) {
    const normalized = text.toLowerCase().replace(/[.\-_]/g, " ");
    const tokens = query
      .toLowerCase()
      .split(/\s+/)
      .filter((t) => t.length > 0);
    return tokens.length > 0 && tokens.every((tok) => normalized.includes(tok));
  }

  // ── State ──

  let transferData = []; // cached from API
  const checkedIds = new Set();

  function getRows() {
    return Array.from(document.querySelectorAll("table.files tbody tr"));
  }

  function updateSelectedCount() {
    const btn = document.getElementById("pm-delete-selected-btn");
    if (btn) {
      const n = checkedIds.size;
      btn.innerHTML = `<span class="fas fa-trash-alt"></span> Delete Selected (${n})`;
      btn.disabled = n === 0;
      btn.style.opacity = n === 0 ? "0.5" : "1";
    }
    // sync Select All checkbox
    const selectAll = document.getElementById("pm-select-all");
    if (selectAll) {
      const rows = getRows();
      selectAll.checked = rows.length > 0 && checkedIds.size === rows.length;
      selectAll.indeterminate =
        checkedIds.size > 0 && checkedIds.size < rows.length;
    }
  }

  function setRowChecked(tr, id, checked) {
    const cb = tr.querySelector(".pm-row-cb");
    if (!cb) return;
    cb.checked = checked;
    if (checked) {
      checkedIds.add(id);
    } else {
      checkedIds.delete(id);
    }
  }

  function getTransferId(tr, index) {
    // Match row to transfer data by index
    return transferData[index] ? transferData[index].id : null;
  }

  // ── Checkboxes ──

  function addCheckboxes() {
    const rows = getRows();
    rows.forEach((tr, i) => {
      if (tr.querySelector(".pm-row-cb")) return;
      const td = document.createElement("td");
      td.style.width = "30px";
      td.style.textAlign = "center";
      const cb = document.createElement("input");
      cb.type = "checkbox";
      cb.className = "pm-row-cb";
      cb.style.cursor = "pointer";
      cb.style.transform = "scale(1.2)";
      cb.addEventListener("change", () => {
        const id = getTransferId(tr, i);
        if (id) {
          if (cb.checked) checkedIds.add(id);
          else checkedIds.delete(id);
        }
        updateSelectedCount();
      });
      td.appendChild(cb);
      tr.prepend(td);
    });
  }

  // ── Toolbar ──

  function addToolbar() {
    if (document.getElementById(TOOLBAR_ID)) return;

    const clearBtn = document.querySelector("button .fas.fa-check-circle");
    if (!clearBtn) {
      setTimeout(addToolbar, 500);
      return;
    }

    const container = clearBtn.closest("button").parentElement;

    const style = document.createElement("style");
    style.textContent = `
      #${TOOLBAR_ID} { display: inline-flex; align-items: center; gap: 6px; margin-right: 8px; }
      #${TOOLBAR_ID} .pm-btn {
        padding: 6px 12px; border: none; border-radius: 4px;
        font-size: 13px; cursor: pointer; white-space: nowrap;
      }
      #${TOOLBAR_ID} .pm-btn:disabled { cursor: not-allowed; }
      #${TOOLBAR_ID} .pm-btn-danger { background: #d9534f; color: #fff; }
      #${TOOLBAR_ID} .pm-btn-danger:hover:not(:disabled) { background: #c9302c; }
      #${TOOLBAR_ID} .pm-btn-warning { background: #f0ad4e; color: #fff; }
      #${TOOLBAR_ID} .pm-btn-warning:hover { background: #ec971f; }
      #${TOOLBAR_ID} .pm-select-all-wrap {
        display: inline-flex; align-items: center; gap: 4px; margin-right: 4px;
      }
      #${TOOLBAR_ID} .pm-select-all-wrap label {
        font-size: 13px; cursor: pointer; margin: 0; user-select: none;
      }
      #${TOOLBAR_ID} .pm-magic-wrap {
        display: inline-flex; align-items: center; gap: 4px;
        background: #f8f9fa; border: 1px solid #ccc; border-radius: 4px;
        padding: 2px 8px;
      }
      #${TOOLBAR_ID} .pm-magic-wrap input {
        border: none; outline: none; background: transparent;
        font-size: 13px; width: 160px; padding: 4px 0;
      }
      #${TOOLBAR_ID} .pm-magic-count {
        font-size: 12px; color: #888; white-space: nowrap;
      }
    `;
    document.head.appendChild(style);

    const toolbar = document.createElement("span");
    toolbar.id = TOOLBAR_ID;

    // Select All
    const selectAllWrap = document.createElement("span");
    selectAllWrap.className = "pm-select-all-wrap";
    const selectAllCb = document.createElement("input");
    selectAllCb.type = "checkbox";
    selectAllCb.id = "pm-select-all";
    selectAllCb.style.cursor = "pointer";
    selectAllCb.style.transform = "scale(1.2)";
    selectAllCb.addEventListener("change", () => {
      const rows = getRows();
      rows.forEach((tr, i) => {
        const id = getTransferId(tr, i);
        if (id) setRowChecked(tr, id, selectAllCb.checked);
      });
      updateSelectedCount();
    });
    const selectAllLabel = document.createElement("label");
    selectAllLabel.htmlFor = "pm-select-all";
    selectAllLabel.textContent = "All";
    selectAllWrap.append(selectAllCb, selectAllLabel);

    // Magic select
    const magicWrap = document.createElement("span");
    magicWrap.className = "pm-magic-wrap";
    const magicIcon = document.createElement("span");
    magicIcon.textContent = "🔮";
    magicIcon.style.fontSize = "14px";
    const magicInput = document.createElement("input");
    magicInput.placeholder = "Magic select...";
    magicInput.id = "pm-magic-input";
    const magicCount = document.createElement("span");
    magicCount.className = "pm-magic-count";
    magicCount.id = "pm-magic-count";

    magicInput.addEventListener("input", () => {
      const query = magicInput.value.trim();
      if (!query) {
        magicCount.textContent = "";
        return;
      }
      const matches = transferData.filter((t) => tokenMatch(query, t.name));
      magicCount.textContent = `${matches.length} match${matches.length !== 1 ? "es" : ""}`;
    });

    magicInput.addEventListener("keydown", (e) => {
      if (e.key === "Enter") {
        e.preventDefault();
        applyMagicSelect(magicInput.value.trim());
      }
    });

    magicWrap.append(magicIcon, magicInput, magicCount);

    // Errored button
    const erroredBtn = document.createElement("button");
    erroredBtn.className = "pm-btn pm-btn-warning";
    erroredBtn.id = "pm-errored-btn";
    erroredBtn.textContent = "⚠ Errored";
    erroredBtn.addEventListener("click", selectErrored);

    // Delete Selected
    const deleteBtn = document.createElement("button");
    deleteBtn.className = "pm-btn pm-btn-danger";
    deleteBtn.id = "pm-delete-selected-btn";
    deleteBtn.innerHTML =
      '<span class="fas fa-trash-alt"></span> Delete Selected (0)';
    deleteBtn.disabled = true;
    deleteBtn.style.opacity = "0.5";
    deleteBtn.addEventListener("click", showDeleteModal);

    toolbar.append(selectAllWrap, magicWrap, erroredBtn, deleteBtn);
    container.prepend(toolbar);
  }

  // ── Actions ──

  function applyMagicSelect(query) {
    if (!query) return;
    checkedIds.clear();
    const rows = getRows();
    let matches = 0;
    rows.forEach((tr, i) => {
      const t = transferData[i];
      if (t) {
        const matched = tokenMatch(query, t.name);
        setRowChecked(tr, t.id, matched);
        if (matched) matches++;
      }
    });
    updateSelectedCount();
  }

  async function selectErrored() {
    // Refresh transfer data
    transferData = await fetchTransfers();
    checkedIds.clear();
    const rows = getRows();
    let errorCount = 0;
    rows.forEach((tr, i) => {
      const t = transferData[i];
      if (t) {
        const isError = t.status === "error";
        setRowChecked(tr, t.id, isError);
        if (isError) errorCount++;
      }
    });
    updateSelectedCount();
  }

  // ── Delete Modal ──

  function createModal() {
    const overlay = document.createElement("div");
    overlay.id = MODAL_ID;
    overlay.innerHTML = `
      <style>
        #${MODAL_ID} .da-overlay {
          position: fixed; inset: 0; background: rgba(0,0,0,0.5);
          z-index: 10000; display: flex; align-items: center; justify-content: center;
        }
        #${MODAL_ID} .da-dialog {
          background: #fff; border-radius: 6px; width: 640px; max-width: 90vw;
          max-height: 80vh; display: flex; flex-direction: column;
          box-shadow: 0 4px 20px rgba(0,0,0,0.3);
        }
        #${MODAL_ID} .da-header {
          padding: 16px 20px; border-bottom: 1px solid #ddd;
          font-size: 18px; font-weight: bold;
        }
        #${MODAL_ID} .da-body {
          padding: 16px 20px; overflow-y: auto; flex: 1;
        }
        #${MODAL_ID} .da-list {
          list-style: none; padding: 0; margin: 8px 0 0 0;
        }
        #${MODAL_ID} .da-list li {
          padding: 4px 0; font-size: 13px; color: #333;
          border-bottom: 1px solid #f0f0f0;
          white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
        }
        #${MODAL_ID} .da-list li::before {
          content: "✕ "; color: #d9534f;
        }
        #${MODAL_ID} .da-progress-wrap {
          margin-top: 12px; display: none;
        }
        #${MODAL_ID} .da-progress-bar-bg {
          background: #e9ecef; border-radius: 4px; height: 24px; overflow: hidden;
        }
        #${MODAL_ID} .da-progress-bar {
          background: #337ab7; height: 100%; width: 0%; transition: width 0.2s;
          display: flex; align-items: center; justify-content: center;
          color: #fff; font-size: 12px; font-weight: bold; min-width: 40px;
        }
        #${MODAL_ID} .da-status {
          margin-top: 8px; font-size: 13px; color: #555;
        }
        #${MODAL_ID} .da-footer {
          padding: 12px 20px; border-top: 1px solid #ddd;
          display: flex; justify-content: flex-end; gap: 8px;
        }
        #${MODAL_ID} .da-btn {
          padding: 8px 20px; border: none; border-radius: 4px;
          font-size: 14px; cursor: pointer;
        }
        #${MODAL_ID} .da-btn-cancel { background: #e9ecef; color: #333; }
        #${MODAL_ID} .da-btn-cancel:hover { background: #d5d8db; }
        #${MODAL_ID} .da-btn-delete { background: #d9534f; color: #fff; }
        #${MODAL_ID} .da-btn-delete:hover { background: #c9302c; }
        #${MODAL_ID} .da-btn:disabled { opacity: 0.5; cursor: not-allowed; }
      </style>
      <div class="da-overlay">
        <div class="da-dialog">
          <div class="da-header">Delete Selected Transfers</div>
          <div class="da-body">
            <div class="da-summary"></div>
            <ul class="da-list"></ul>
            <div class="da-progress-wrap">
              <div class="da-progress-bar-bg">
                <div class="da-progress-bar">0%</div>
              </div>
              <div class="da-status"></div>
            </div>
          </div>
          <div class="da-footer">
            <button class="da-btn da-btn-cancel">Cancel</button>
            <button class="da-btn da-btn-delete">Delete All</button>
          </div>
        </div>
      </div>
    `;
    document.body.appendChild(overlay);

    const el = (sel) => overlay.querySelector(sel);
    el(".da-btn-cancel").addEventListener("click", () => overlay.remove());
    el(".da-overlay").addEventListener("click", (e) => {
      if (e.target === el(".da-overlay")) overlay.remove();
    });

    return {
      overlay,
      summary: el(".da-summary"),
      list: el(".da-list"),
      progressWrap: el(".da-progress-wrap"),
      progressBar: el(".da-progress-bar"),
      status: el(".da-status"),
      cancelBtn: el(".da-btn-cancel"),
      deleteBtn: el(".da-btn-delete"),
    };
  }

  async function showDeleteModal() {
    if (checkedIds.size === 0) return;

    const existing = document.getElementById(MODAL_ID);
    if (existing) existing.remove();

    const selected = transferData.filter((t) => checkedIds.has(t.id));
    if (selected.length === 0) return;

    const m = createModal();
    m.summary.textContent = `Delete ${selected.length} transfer(s):`;
    selected.forEach((t) => {
      const li = document.createElement("li");
      const extra = t.status === "error" && t.message ? `  —  ${t.message.trim()}` : "";
      li.textContent = `${t.name}${extra}`;
      li.title = t.name;
      m.list.appendChild(li);
    });

    m.deleteBtn.addEventListener("click", async () => {
      m.deleteBtn.disabled = true;
      m.cancelBtn.disabled = true;
      m.list.style.display = "none";
      m.progressWrap.style.display = "block";

      let deleted = 0;
      let failed = 0;

      for (const t of selected) {
        m.status.textContent = `Deleting: ${t.name}`;
        try {
          const result = await deleteTransfer(t.id);
          if (result.status !== "success") {
            console.warn("Delete failed:", t.name, result);
            failed++;
          }
        } catch (e) {
          console.error("Delete error:", t.name, e);
          failed++;
        }
        deleted++;
        const pct = Math.round((deleted / selected.length) * 100);
        m.progressBar.style.width = pct + "%";
        m.progressBar.textContent = pct + "%";
        await sleep(DELAY_MS);
      }

      const msg =
        failed > 0
          ? `Done. Deleted ${deleted - failed}/${selected.length} (${failed} failed).`
          : `Done. Deleted all ${selected.length} transfers.`;
      m.status.textContent = msg;
      m.summary.textContent = msg;
      m.cancelBtn.disabled = false;
      m.cancelBtn.textContent = "Close";
      m.deleteBtn.style.display = "none";

      setTimeout(() => location.reload(), 2000);
    });
  }

  // ── Init ──

  async function init() {
    transferData = await fetchTransfers();

    addToolbar();
    addCheckboxes();

    // Update errored count on button
    const errorCount = transferData.filter((t) => t.status === "error").length;
    const erroredBtn = document.getElementById("pm-errored-btn");
    if (erroredBtn) {
      erroredBtn.textContent = `⚠ Errored (${errorCount})`;
    }
  }

  // Wait for the table to be ready
  function waitAndInit() {
    if (document.querySelector("table.files tbody tr")) {
      init();
    } else {
      setTimeout(waitAndInit, 500);
    }
  }

  waitAndInit();
})();
