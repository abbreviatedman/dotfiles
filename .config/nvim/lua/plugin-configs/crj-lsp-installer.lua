local lsp_installer = require("nvim-lsp-installer")
require("plugin-configs.crj-lspconfig")

lsp_installer.settings {
  ui = {
    icons = {
      server_installed = "✔",
      server_pending = "➡",
      server_uninstalled="X"
    }
  },

  max_concurrent_installers = 4
}

local function make_server_ready(attach)
  lsp_installer.on_server_ready(function(server)
    local options = {}
    options.on_attach = attach
    server:setup(options)
    vim.cmd [[ do User LspAttachBuffers ]]
  end)
end

local function install_server(server)
  local lsp_installer_servers = require('nvim-lsp-installer.servers')
  local ok, server_analyzer = lsp_installer_servers.get_server(server)
  if ok then
    if not server_analyzer:is_installed() then
      server_analyzer:install(server)
    end
  end
end

local servers = {
  "tsserver",
  "jsonls",
}

make_server_ready(On_attach)
for _, server in ipairs(servers) do
  install_server(server)
end
