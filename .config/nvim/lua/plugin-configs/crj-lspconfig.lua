vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    underline = true,
    signs = true,
    update_in_insert = true,
    virtual_text = {
      true,
      spacing = 6,
      -- severity_limit = error
    }
  }
)

On_attach = function(_, buffer_number)
  local function set_buffer_keymap(...) map_in_current_buffer(buffer_number, ...) end
  vim.api.nvim_buf_set_option(buffer_number, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  set_buffer_keymap('n', '[e', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')
  set_buffer_keymap('n', ']e', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>')
  set_buffer_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>')
  set_buffer_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>')
  set_buffer_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>')
  set_buffer_keymap('n', '<Leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>')
  set_buffer_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>')
end
