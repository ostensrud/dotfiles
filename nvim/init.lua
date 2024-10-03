require("config.lazy")

vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2

local builtin = require("telescope.builtin")

vim.keymap.set("n", "<localleader>ff", builtin.find_files, {})
vim.keymap.set("n", "<localleader>fg", builtin.live_grep, {})
vim.keymap.set("n", "<localleader>fb", builtin.buffers, {})
vim.keymap.set("n", "<localleader>fh", builtin.help_tags, {})
vim.keymap.set("n", "<a-cr>", vim.lsp.buf.code_action, {})

vim.keymap.set("n", "gD", vim.lsp.buf.declaration, {})
vim.keymap.set("n", "gd", vim.lsp.buf.definition, {})
vim.keymap.set("n", "gr", vim.lsp.buf.references, {})

vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    -- Only open NeoTree if no files were passed as arguments
    if #vim.fn.argv() == 0 then
      require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd() })
    end
  end,
})
