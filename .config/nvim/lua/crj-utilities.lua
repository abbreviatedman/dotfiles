function map (mode, key, result, options)
  if (options)
    then vim.api.nvim_set_keymap(
      mode,
      key,
      result,
      options
    ) else vim.api.nvim_set_keymap(
		mode,
		key,
		result,
		{noremap = true, silent = true}
	) end
end

function map_in_current_buffer (buffer_number, mode, key, result, options)
  if (options)
    then vim.api.nvim_buf_set_keymap(
      buffer_number,
      mode,
      key,
      result,
      options
    ) else vim.api.nvim_buf_set_keymap(
    buffer_number,
		mode,
		key,
		result,
		{noremap = true, silent = true}
	) end
end

local is_night = true
function toggle_night()
  if (is_night) then
    is_night = false
    vim.api.nvim_set_var('modus_termtrans_enable', 0)
    vim.cmd('colorscheme modus-operandi')
  else
    is_night = true
    vim.api.nvim_set_var('modus_termtrans_enable', 1)
    vim.cmd('colorscheme modus-vivendi')
  end
end

-- Uncomment the below line to default to light mode instead of dark.
-- toggle_night()
