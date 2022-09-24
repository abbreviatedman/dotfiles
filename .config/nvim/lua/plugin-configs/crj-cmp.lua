local cmp = require'cmp'


cmp.setup{
  completion = {
    -- completeopt = 'menu,menuone,noinsert' -- alternate completion options
  },

  snippet = {
    expand = function(args)
      require'luasnip'.lsp_expand(args.body)
    end
  },

  formatting = {
    format = function(entry, item)
      item.kind = require'lspkind'.presets.default[item.kind]
      item.menu = ({
        buffer = '[Buff]',
        nvim_lsp = '[LSP]',
        luasnip = '[LuaSnip]',
        nvim_lua = '[Lua]',
        latex_symbols = '[Latex]',
      })[entry.source.name]

      return item
    end
  }
}
