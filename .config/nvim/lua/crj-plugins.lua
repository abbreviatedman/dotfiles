return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- [[ LSP ]]
  use {
    'neovim/nvim-lspconfig',
    config = [[ require('plugin-configs/crj-lspconfig') ]]
  }

  use {
    'williamboman/nvim-lsp-installer',
    config = [[ require('plugin-configs/crj-lsp-installer') ]]
  }

  use {
    'onsails/lspkind-nvim',
    config = [[ require('plugin-configs/crj-lspkind') ]]
  }

  use {
    'git@github.com:hrsh7th/nvim-cmp',
    requires = {
      'git@github.com:hrsh7th/cmp-nvim-lsp',
      'git@github.com:hrsh7th/cmp-nvim-lua',
      'git@github.com:hrsh7th/cmp-path',
    },

    config = [[ require('plugin-configs/crj-cmp') ]]
  }

  use {
    'L3mon4D3/LuaSnip',
    require = {
      'rafamadriz/friendly-snippets',
    },
  }

  -- use {
  --   'nvim-lua/lsp-status.nvim',
  --   config = [[ require('plugin-configs/crj-lsp-status') ]]
  -- }

  -- Theme
  use {
    'ishan9299/modus-theme-vim',
  }

  use {
    'git@github.com:projekt0n/github-nvim-theme.git',
    -- config = function() require('github-theme').setup({
    --   theme_style = "light_colorblind",
    --   keyword_style="italic",
    --   variable_style="italic"
    -- }) end
  }

  -- File Explorer
  use {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require('nvim-tree').setup{
      view = {
        -- this appears to have been deprecated
        -- auto_resize = true,
        number = true,
        relativenumber = true
      },

      actions = {
        open_file = {
          quit_on_open = true
        }
      }
    } end
  }

  use {
    'nvim-telescope/telescope-fzf-native.nvim',
    run = 'make',
    config = function() require('telescope').load_extension('fzf') end
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  use {
    "nvim-telescope/telescope-file-browser.nvim",
    config = function()
      require('telescope').setup()
      require('telescope').load_extension('file_browser')
    end
  }
  -- require("telescope").setup {
  -- extensions = {
  --   file_browser = {
  --     theme = "ivy",
  --     mappings = {
  --       ["i"] = {
  --         -- your custom insert mode mappings
  --       },
  --       ["n"] = {
  --         -- your custom normal mode mappings
  --       },
  --     },
  --   },
  -- },
-- }

-- To get telescope-file-browser loaded and working with telescope,
-- you need to call load_extension, somewhere after setup function:
-- require("telescope").load_extension "file_browser"

  use 'jiangmiao/auto-pairs'
  use 'justinmk/vim-sneak'
  use 'tpope/vim-repeat'
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'

end
)
