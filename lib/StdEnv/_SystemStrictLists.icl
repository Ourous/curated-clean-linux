implementation module _SystemStrictLists;

class List .l e where {
	_cons :: .e u:(l .e) -> u:(l .e);
	_decons :: !u:(l .e) -> (.e,u:(l .e));
};

class UList e where {
	_cons_u :: !.e u:[#.e] -> u:[#.e];
	_decons_u :: !u:[#.e] -> (!.e,u:[#.e]);
};

class UTSList e where {
	_cons_uts :: !.e !u:[#.e!] -> u:[#.e!];
	_decons_uts :: !u:[#.e!] -> (!.e,!u:[#.e!]);
};

_nil :: u:(l .e) | List l e , [u<=e];
_nil = code {
		pop_a 2
		fillh _Nil 0 0
	};

_nil_u :: .[#.e] | UList e;
_nil_u = code {
		pop_a 2
		fillh _Nil 0 0
	};

_nil_uts :: .[#.e!] | UTSList e;
_nil_uts = code {
		pop_a 2
		fillh _Nil 0 0
	};

instance List [] a where {
	_cons a b = [a:b];
	_decons [a:b] = (a,b);
}

instance List [!] a where {
	_cons a b = [!a:b];
	_decons [!a:b] = (a,b);
}

instance List [ !] a where {
	_cons a b = [a:b!];
	_decons [a:b!] = (a,b);
}

instance List [!!] a where {
	_cons a b = [!a:b!];
	_decons [!a:b!] = (a,b);
}

instance List [#] a | UList a where {
	_cons a b = _cons_u a b;
	_decons a = _decons_u a;
}

instance List [#!] a | UTSList a where {
	_cons a b = _cons_uts a b;
	_decons a = _decons_uts a;
}

instance UList a where {
	_cons_u a b = code {
		print "_cons_u of UList a"
		halt
	};
	_decons_u _ = code {
		print "_decons_u of UList a"
		halt
	}
}	

instance UList Int where {
	_cons_u a b = code {
		fill_r _Consi 1 1 1 0 0
		pop_b 1
		pop_a 1
	};
	_decons_u a = code {
		repl_r_args 1 1
	}
}

instance UList Real where {
	_cons_u a b = code {
		fill_r _Consr 1 2 1 0 0
		pop_b 2
		pop_a 1
	};
	_decons_u a = code {
		repl_r_args 1 2
	}
}

instance UList Char where {
	_cons_u a b = code {
		fill_r _Consc 1 1 1 0 0
		pop_b 1
		pop_a 1
	};
	_decons_u a = code {
		repl_r_args 1 1
	}
}

instance UList Bool where {
	_cons_u a b = code {
		fill_r _Consb 1 1 1 0 0
		pop_b 1
		pop_a 1
	};
	_decons_u a = code {
		repl_r_args 1 1
	}
}

instance UList File where {
	_cons_u a b = code {
		fill_r _Consf 1 2 1 0 0
		pop_b 2
		pop_a 1
	};
	_decons_u a = code {
		repl_r_args 1 2
	}
}

instance UList {.a} where {
	_cons_u a b = code {
		fill_r _Consa 2 0 2 0 0
		pop_a 2
	};
	_decons_u a = code {
		repl_r_args 2 0
	}
}

instance UList {!.a} where {
	_cons_u a b = code {
		fill_r _Consa 2 0 2 0 0
		pop_a 2
	};
	_decons_u a = code {
		repl_r_args 2 0
	}
}

instance UList {#.a} where {
	_cons_u a b = code {
		fill_r _Consa 2 0 2 0 0
		pop_a 2
	};
	_decons_u a = code {
		repl_r_args 2 0
	}
}

instance UTSList a where {
	_cons_uts a b = code {
		print "_cons_uts of UTSList a"
		halt
	};
	_decons_uts _ = code {
		print "_decons_uts of UTSList a"
		halt
	}
}	

instance UTSList Int where {
	_cons_uts a b = code {
		fill_r _Consits 1 1 1 0 0
		pop_b 1
		pop_a 1
	};
	_decons_uts a = code {
		repl_r_args 1 1
	}
}

instance UTSList Real where {
	_cons_uts a b = code {
		fill_r _Consrts 1 2 1 0 0
		pop_b 2
		pop_a 1
	};
	_decons_uts a = code {
		repl_r_args 1 2
	}
}

instance UTSList Char where {
	_cons_uts a b = code {
		fill_r _Conscts 1 1 1 0 0
		pop_b 1
		pop_a 1
	};
	_decons_uts a = code {
		repl_r_args 1 1
	}
}

instance UTSList Bool where {
	_cons_uts a b = code {
		fill_r _Consbts 1 1 1 0 0
		pop_b 1
		pop_a 1
	};
	_decons_uts a = code {
		repl_r_args 1 1
	}
}

instance UTSList File where {
	_cons_uts a b = code {
		fill_r _Consfts 1 2 1 0 0
		pop_b 2
		pop_a 1
	};
	_decons_uts a = code {
		repl_r_args 1 2
	}
}

instance UTSList {.a} where {
	_cons_uts a b = code {
		fill_r _Consa 2 0 2 0 0
		pop_a 2
	};
	_decons_uts a = code {
		repl_r_args 2 0
	}
}

instance UTSList {!.a} where {
	_cons_uts a b = code {
		fill_r _Consa 2 0 2 0 0
		pop_a 2
	};
	_decons_uts a = code {
		repl_r_args 2 0
	}
}

instance UTSList {#.a} where {
	_cons_uts a b = code {
		fill_r _Consa 2 0 2 0 0
		pop_a 2
	};
	_decons_uts a = code {
		repl_r_args 2 0
	}
}
