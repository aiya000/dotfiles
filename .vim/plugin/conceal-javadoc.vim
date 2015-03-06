" Forked from https://gist.github.com/mfumi/9814303
"             http://d.hatena.ne.jp/mFumi/20140328/1395946070

if has('conceal')
	function! s:concealJavaDocTags()
		syn clear javaDocTags
		syn clear htmlTag
		syn clear htmlEndTag
		syn clear htmlSpecialChar
		syn clear htmlLink
		syn clear htmlBold
		syn clear htmlBold
		syn clear htmlBoldUnderline
		syn clear htmlBoldItalic
		syn clear htmlBoldItalic
		syn clear htmlBoldUnderlineItalic
		syn clear htmlBoldUnderlineItalic
		syn clear htmlBoldItalicUnderline
		syn clear htmlUnderline
		syn clear htmlUnderlineBold
		syn clear htmlUnderlineBold
		syn clear htmlUnderlineItalic
		syn clear htmlUnderlineItalic
		syn clear htmlUnderlineItalicBold
		syn clear htmlUnderlineItalicBold
		syn clear htmlUnderlineBoldItalic
		syn clear htmlUnderlineBoldItalic
		syn clear htmlItalic
		syn clear htmlItalic
		syn clear htmlItalicBold
		syn clear htmlItalicBold
		syn clear htmlItalicBoldUnderline
		syn clear htmlItalicUnderline
		syn clear htmlItalicUnderlineBold
		syn clear htmlItalicUnderlineBold
	
		syn region javaDocTags contained concealends matchgroup=javaDocTags start="{@\(code\|link\|linkplain\|inherit[Dd]oc\|doc[rR]oot\|value\) " matchgroup=concealEnd end="}"
		syn match  javaDocTags	contained "@\(param\|exception\|throws\|since\)\s\+\S\+" contains=javaDocParam
		syn match  javaDocTags	 contained "@\(version\|author\|return\|deprecated\|serial\|serialField\|serialData\)\>"
		syn match concealEnd conceal "*"
	
		syn region  htmlTag      contained conceal    start=+<[^/]+   end=+>+ fold contains=htmlTagN,htmlArg,htmlValue,htmlTagError,htmlEvent,htmlCssDefinition,@htmlPreproc,@htmlArgCluster
		syn region  htmlEndTag   contained conceal    start=+</+      end=+>+ contains=htmlTagN,htmlTagError
		syn match htmlSpecialChar contained "&#\=[0-9A-Za-z]\{1,8};"
		syn match htmlSpecialChar contained conceal cchar=  "&nbsp;"
		syn match htmlSpecialChar contained conceal cchar=< "&lt;"
		syn match htmlSpecialChar contained conceal cchar=> "&gt;"
		syn region htmlTitle start="<tt\>" end="</tt>"me=e-5 contains=@htmlTop     concealends
		syn region htmlTitle start="<code\>" end="</code>"me=e-7 contains=@htmlTop concealends
		syn region htmlLink contained concealends start="<a\>\_[^>]*\<href\>" end="</a>"me=e-4 contains=@Spell,htmlTag,htmlEndTag,htmlSpecialChar,htmlPreProc,htmlComment,htmlLeadingSpace,javaScript,@htmlPreproc
	
		syn region htmlBold start="<b\>" end="</b>"me=e-4 contains=@htmlTop,htmlBoldUnderline,htmlBoldItalic                        concealends
		syn region htmlBold start="<strong\>" end="</strong>"me=e-9 contains=@htmlTop,htmlBoldUnderline,htmlBoldItalic              concealends
		syn region htmlBoldUnderline contained start="<u\>" end="</u>"me=e-4 contains=@htmlTop,htmlBoldUnderlineItalic              concealends
		syn region htmlBoldItalic contained start="<i\>" end="</i>"me=e-4 contains=@htmlTop,htmlBoldItalicUnderline                 concealends
		syn region htmlBoldItalic contained start="<em\>" end="</em>"me=e-5 contains=@htmlTop,htmlBoldItalicUnderline               concealends
		syn region htmlBoldUnderlineItalic contained start="<i\>" end="</i>"me=e-4 contains=@htmlTop                                concealends
		syn region htmlBoldUnderlineItalic contained start="<em\>" end="</em>"me=e-5 contains=@htmlTop                              concealends
		syn region htmlBoldItalicUnderline contained start="<u\>" end="</u>"me=e-4 contains=@htmlTop,htmlBoldUnderlineItalic        concealends
	
		syn region htmlUnderline start="<u\>" end="</u>"me=e-4 contains=@htmlTop,htmlUnderlineBold,htmlUnderlineItalic              concealends
		syn region htmlUnderlineBold contained start="<b\>" end="</b>"me=e-4 contains=@htmlTop,htmlUnderlineBoldItalic              concealends
		syn region htmlUnderlineBold contained start="<strong\>" end="</strong>"me=e-9 contains=@htmlTop,htmlUnderlineBoldItalic    concealends
		syn region htmlUnderlineItalic contained start="<i\>" end="</i>"me=e-4 contains=@htmlTop,htmlUnderlineItalicBold            concealends
		syn region htmlUnderlineItalic contained start="<em\>" end="</em>"me=e-5 contains=@htmlTop,htmlUnderlineItalicBold          concealends
		syn region htmlUnderlineItalicBold contained start="<b\>" end="</b>"me=e-4 contains=@htmlTop                                concealends
		syn region htmlUnderlineItalicBold contained start="<strong\>" end="</strong>"me=e-9 contains=@htmlTop                      concealends
		syn region htmlUnderlineBoldItalic contained start="<i\>" end="</i>"me=e-4 contains=@htmlTop                                concealends
		syn region htmlUnderlineBoldItalic contained start="<em\>" end="</em>"me=e-5 contains=@htmlTop                              concealends
	
		syn region htmlItalic start="<i\>" end="</i>"me=e-4 contains=@htmlTop,htmlItalicBold,htmlItalicUnderline                    concealends
		syn region htmlItalic start="<em\>" end="</em>"me=e-5 contains=@htmlTop                                                     concealends
		syn region htmlItalicBold contained start="<b\>" end="</b>"me=e-4 contains=@htmlTop,htmlItalicBoldUnderline                 concealends
		syn region htmlItalicBold contained start="<strong\>" end="</strong>"me=e-9 contains=@htmlTop,htmlItalicBoldUnderline       concealends
		syn region htmlItalicBoldUnderline contained start="<u\>" end="</u>"me=e-4 contains=@htmlTop                                concealends
		syn region htmlItalicUnderline contained start="<u\>" end="</u>"me=e-4 contains=@htmlTop,htmlItalicUnderlineBold            concealends
		syn region htmlItalicUnderlineBold contained start="<b\>" end="</b>"me=e-4 contains=@htmlTop                                concealends
		syn region htmlItalicUnderlineBold contained start="<strong\>" end="</strong>"me=e-9 contains=@htmlTop                      concealends
		hi Conceal NONE
		hi link htmlTitle String
		let b:conceallevel_save = &conceallevel
		setl conceallevel=2
	endfunction
	
	function! s:toggleConceal()
		if !exists('b:conceallevel_save') || &conceallevel == b:conceallevel_save
			if &conceallevel == 2
				let b:conceallevel_save = &conceallevel
				let &conceallevel = 0
			else
				let b:conceallevel_save = &conceallevel
				let &conceallevel = 2
			endif
		else
			let l:tmp = &conceallevel
			let &conceallevel = b:conceallevel_save
			let b:conceallevel_save = l:tmp
		endif
	endfunction
	

	augroup ProgramTypes
		autocmd FileType java call s:concealJavaDocTags()
	augroup END
	
	command! -nargs=0 ToggleConceal call s:toggleConceal()
endif
