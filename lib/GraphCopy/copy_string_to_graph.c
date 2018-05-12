
#include <stdlib.h>

#ifdef _WIN64
# define Int __int64
# define INT_descriptor dINT
# define ARCH_64 1
#else
# if defined (MACH_O64) || defined (LINUX64)
#  define Int long long
#  define INT_descriptor dINT
#  define ARCH_64 1
# else
#  define Int int
#  define INT_descriptor INT
#  define ARCH_64 0
# endif
# if !(defined (LINUX32) || defined (LINUX64))
#  define __STRING__ _STRING__
#  define __ARRAY__ _ARRAY__
# endif
#endif

#if defined (MACH_O64) || defined (PIC)
// Use positions relative to _ARRAY_ for address space layout randomization systems.
#  define USE_DESC_RELATIVE_TO_ARRAY 1
#endif

extern void *INT_descriptor,*CHAR,*BOOL,*REAL,*__STRING__,*__ARRAY__;
extern Int small_integers[],static_characters[];

/*inline*/
static void copy (Int *dest_p,Int *source_p,Int n_words)
{
	Int i;

	for (i=0; i<n_words; ++i)
		dest_p[i]=source_p[i];
}

Int *copy_string_to_graph (Int *string_p,void *begin_free_heap,void *end_free_heap,Int **last_heap_pa)
{
	Int ***stack_p,***stack_begin,***stack_end,*heap_p,**arg_a,*root_node_p,n_free_words;

	string_p+=2;

	stack_end=end_free_heap;
	stack_begin=end_free_heap;
	stack_p=end_free_heap;
		
	heap_p=begin_free_heap;
	
	n_free_words=(Int*)end_free_heap-(Int*)begin_free_heap;
	arg_a=&root_node_p;

	for (;;){
		for (;;){
			Int desc;
			
			desc=*string_p;
			
			if (--n_free_words<0){
				*last_heap_pa=heap_p+1+(stack_end-stack_begin);
				return (Int*)((Int)string_p+1);
			}

			if (!(desc & 1)){
				*string_p=(Int)heap_p;
				*arg_a=heap_p;
#ifdef USE_DESC_RELATIVE_TO_ARRAY
				desc+=(Int)&__ARRAY__;
#endif
				*heap_p=desc;
				if (desc & 2){
					unsigned Int arity;
					
					arity=((unsigned short *)desc)[-1];
					if (arity==0){
						if (desc==(Int)&INT_descriptor+2){
							Int i;
							
							i=string_p[1];
							if ((unsigned Int)i<=(unsigned Int)32){
								Int *a;
								
								a=&small_integers[i<<1];
								++n_free_words;
								*arg_a=a;
								*string_p=(Int)a;
							} else {
								if (--n_free_words<0){
									*last_heap_pa=heap_p+2+(stack_end-stack_begin);
									return (Int*)((Int)&string_p[1]+1);
								}
								
								heap_p[1]=i;
								heap_p+=2;
							}
							string_p+=2;
							break;
						} else if (desc==(Int)&CHAR+2){
							unsigned char c;
							Int *a;
							
							c=(unsigned char)(string_p[1]);
							a=&static_characters[(Int)c<<1];
							++n_free_words;
							*arg_a=a;
							*string_p=(Int)a;
							string_p+=2;
							break;
						} else if (desc==(Int)&BOOL+2
#if ARCH_64
							|| desc==(Int)&REAL+2
#endif
						){
							if (--n_free_words<0){
								*last_heap_pa=heap_p+2+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}
							
							heap_p[1]=string_p[1];
							string_p+=2;
							heap_p+=2;
							break;
#if ! ARCH_64
						} else if (desc==(Int)&REAL+2){
							n_free_words-=2;
							if (n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}
							
							heap_p[1]=string_p[1];
							heap_p[2]=string_p[2];
							string_p+=3;
							heap_p+=3;
							break;
#endif
						} else if (desc==(Int)&__STRING__+2){
							unsigned Int length,n_words;
							
							length=string_p[1];
							string_p+=2;
#if ARCH_64
							n_words=(length+7)>>3;
#else
							n_words=(length+3)>>2;
#endif							
							n_free_words-=n_words+1;
							if (n_free_words<0){
								*last_heap_pa=heap_p+2+(stack_end-stack_begin);
								return (Int*)((Int)string_p+1);
							}
													
							heap_p[1]=length;
							heap_p+=2;

							copy (heap_p,string_p,n_words);
							string_p+=n_words;
							heap_p+=n_words;
							break;
						} else if (desc==(Int)&__ARRAY__+2){
							Int array_size,elem_desc;

							n_free_words-=2;
							if (n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}
							
							array_size=string_p[1];
							elem_desc=string_p[2];
							string_p+=3;
#if defined (MACH_O64)
							if (elem_desc!=0)
								elem_desc+=(Int)&__ARRAY__;
#endif
							heap_p[1]=array_size;
							heap_p[2]=elem_desc;
							heap_p+=3;
													
							if (elem_desc==0){
								Int i;
								
								stack_p-=array_size;
								if (stack_p<stack_begin){
									Int extra_words;

									extra_words=stack_begin-stack_p;
									n_free_words-=extra_words;
									if (n_free_words<0){
										*last_heap_pa=heap_p+array_size+(stack_end-stack_p);
										return (Int*)((Int)string_p+1);
									}
									stack_begin=stack_p;
								}
								
								i=array_size;
								while (--i>=0)
									stack_p[i]=(Int**)&heap_p[i];

								heap_p+=array_size;
								break;
							} else if (elem_desc==(Int)&INT_descriptor+2
#if ARCH_64
								|| elem_desc==(Int)&REAL+2
#endif
							){
								n_free_words-=array_size;
								if (n_free_words<0){
									*last_heap_pa=heap_p+array_size+(stack_end-stack_begin);
									return (Int*)((Int)string_p+1);
								}

								copy (heap_p,string_p,array_size);
								string_p+=array_size;
								heap_p+=array_size;
								break;
#if ! ARCH_64
							} else if (elem_desc==(Int)&REAL+2){
								array_size<<=1;
							
								n_free_words-=array_size;
								if (n_free_words<0){
									*last_heap_pa=heap_p+array_size+(stack_end-stack_begin);
									return (Int*)((Int)string_p+1);
								}
								
								copy (heap_p,string_p,array_size);
								string_p+=array_size;
								heap_p+=array_size;
								break;
#endif
							} else if (elem_desc==(Int)&BOOL+2){
#if ARCH_64
								array_size=(array_size+7)>>3;
#else
								array_size=(array_size+3)>>2;
#endif
								n_free_words-=array_size;
								if (n_free_words<0){
									*last_heap_pa=heap_p+array_size+(stack_end-stack_begin);
									return (Int*)((Int)string_p+1);
								}
								
								copy (heap_p,string_p,array_size);
								string_p+=array_size;
								heap_p+=array_size;
								break;
							} else {
								Int n_field_pointers,field_size;

								n_field_pointers=*(unsigned short *)elem_desc;
								field_size=((unsigned short *)elem_desc)[-1]-(Int)256;

								if (n_field_pointers==0){
									array_size*=field_size;
									
									n_free_words-=array_size;
									if (n_free_words<0){
										*last_heap_pa=heap_p+array_size+(stack_end-stack_begin);
										return (Int*)((Int)string_p+1);
									}
																		
									copy (heap_p,string_p,array_size);
									string_p+=array_size;
									heap_p+=array_size;
									break;
								} else if (n_field_pointers==field_size){
									Int i;
									
									array_size*=field_size;

									stack_p-=array_size;
									if (stack_p<stack_begin){
										Int extra_words;

										extra_words=stack_begin-stack_p;
										n_free_words-=extra_words;
										if (n_free_words<0){
											*last_heap_pa=heap_p+array_size+(stack_end-stack_p);
											return (Int*)((Int)string_p+1);
										}
										stack_begin=stack_p;
									}
									
									i=array_size;
									while (--i>=0)
										stack_p[i]=(Int**)&heap_p[i];
									
									heap_p+=array_size;
									break;
								} else {
									Int n_non_field_pointers,i,***pointer_p;
									
									n_non_field_pointers=field_size-n_field_pointers;
									
									n_free_words-=array_size*field_size;
									if (n_free_words<0){
										*last_heap_pa=heap_p+array_size*field_size+(stack_end-stack_begin);
										return (Int*)((Int)string_p+1);
									}
									
									stack_p-=array_size*n_field_pointers;
									if (stack_p<stack_begin){
										Int extra_words;

										extra_words=stack_begin-stack_p;
										n_free_words-=extra_words;
										if (n_free_words<0){
											*last_heap_pa=heap_p+(stack_end-stack_p);
											return (Int*)((Int)string_p+1);
										}
										stack_begin=stack_p;
									}
									
									pointer_p=stack_p;
									
									for (i=0; i<array_size; ++i){
										Int n;
										
										n=n_field_pointers;
										while (--n>=0)
											pointer_p[n]=(Int**)&heap_p[n];
										pointer_p+=n_field_pointers;
										heap_p+=n_field_pointers;
										
										copy (heap_p,string_p,n_non_field_pointers);
										heap_p+=n_non_field_pointers;
										string_p+=n_non_field_pointers;						
									}
									break;
								}
							}
						} else {
#ifdef OLD_DESCRIPTORS
							desc-=10;
#else
# if ARCH_64
							desc-=10;
# else
							desc-=6;
# endif
#endif
							++n_free_words;
							*arg_a=(Int*)desc;
							*string_p=desc;
							++string_p;
							break;
						}
					} else if (arity==1){
						if (--n_free_words<0){
							*last_heap_pa=heap_p+2+(stack_end-stack_begin);
							return (Int*)((Int)&string_p[1]+1);
						}
						arg_a=(Int**)&heap_p[1];
						++string_p;
						heap_p+=2;
						continue;
					} else if (arity==2){
						n_free_words-=2;
						if (n_free_words<0){
							*last_heap_pa=heap_p+3+(stack_end-stack_begin);
							return (Int*)((Int)&string_p[1]+1);
						}
						
						if (stack_p<=stack_begin){
							if (--n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-1-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}
							--stack_begin;
						}
						
						*--stack_p=(Int**)&heap_p[2];
						arg_a=(Int**)&heap_p[1];
						++string_p;
						heap_p+=3;
						continue;
					} else if (arity<256){
						Int n_words;

						n_free_words-=arity+1;
						if (n_free_words<0){
							*last_heap_pa=heap_p+arity+2+(stack_end-stack_begin);
							return (Int*)((Int)&string_p[1]+1);
						}

						arg_a=(Int**)&heap_p[1];
						heap_p[2]=(Int)&heap_p[3];
						heap_p+=3;

						n_words=arity-1;

						stack_p-=n_words;
						if (stack_p<stack_begin){
							Int extra_words;

							extra_words=stack_begin-stack_p;
							n_free_words-=extra_words;
							if (n_free_words<0){
								*last_heap_pa=heap_p+arity-1+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}
							stack_begin=stack_p;
						}

						--n_words;
						stack_p[n_words]=(Int**)&heap_p[n_words];
						--n_words;
						stack_p[n_words]=(Int**)&heap_p[n_words];
						while (--n_words>=0)
							stack_p[n_words]=(Int**)&heap_p[n_words];

						heap_p+=arity-1;
						++string_p;
						continue;
					} else {
						Int n_pointers;
						
						n_pointers=*(unsigned short*)desc;
						arity-=256;
						
						if (arity==1){
							if (--n_free_words<0){
								*last_heap_pa=heap_p+2+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}
							
							if (n_pointers==0){						
								heap_p[1]=string_p[1];
								string_p+=2;
								heap_p+=2;
								break;
							} else {
								arg_a=(Int**)&heap_p[1];
								++string_p;
								heap_p+=2;
								continue;
							}
						} else if (arity==2){
							n_free_words-=2;
							if (n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}

							if (n_pointers==0){
								heap_p[1]=string_p[1];
								heap_p[2]=string_p[2];
								string_p+=3;
								heap_p+=3;
								break;
							} else {
								if (n_pointers==1){
									heap_p[2]=string_p[1];
									string_p+=2;
								} else {
									if (stack_p<=stack_begin){
										if (--n_free_words<0){
											*last_heap_pa=heap_p+3+1+(stack_end-stack_begin);
											return (Int*)((Int)&string_p[1]+1);
										}
										--stack_begin;
									}
									++string_p;
									*--stack_p=(Int**)&heap_p[2];
								}
								arg_a=(Int**)&heap_p[1];
								heap_p+=3;
								continue;							
							}
						} else {
							n_free_words-=arity+1;
							if (n_free_words<0){
								*last_heap_pa=heap_p+arity+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}
							
							heap_p[2]=(Int)&heap_p[3];
							
							if (n_pointers==0){
								heap_p[1]=string_p[1];
								heap_p+=3;
								string_p+=2;
								--arity;
								copy (heap_p,string_p,arity);
								string_p+=arity;
								heap_p+=arity;
								break;
							} else {
								Int n_non_pointers;

								arg_a=(Int**)&heap_p[1];
								heap_p+=3;

								n_non_pointers=arity-n_pointers;
								++string_p;

								if (n_non_pointers>0){
									Int *non_pointers_p;

									non_pointers_p=&heap_p[n_pointers-1];

									copy (non_pointers_p,string_p,n_non_pointers);
									string_p+=n_non_pointers;
								}
								
								--n_pointers;
								if (n_pointers>0){
									Int i;
									
									stack_p-=n_pointers;
									if (stack_p<stack_begin){
										Int extra_words;

										extra_words=stack_begin-stack_p;
										n_free_words-=extra_words;
										if (n_free_words<0){
											*last_heap_pa=heap_p+n_pointers+n_non_pointers+(stack_end-stack_p);
											return (Int*)((Int)string_p+1);
										}
										stack_begin=stack_p;
									}
									
									i=n_pointers;
									while (--i>=0)
										stack_p[i]=(Int**)&heap_p[i];
								}
								heap_p+=n_pointers+n_non_pointers;
								continue;
							}
						}
					}
				} else {
					Int arity;

					arity=((int*)desc)[-1];
					if (arity>1){
						if (arity<256){
							Int n_words;
							
							n_free_words-=arity;
							if (n_free_words<0){
								*last_heap_pa=heap_p+arity+1+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}

							n_words=arity-1;

							stack_p-=n_words;
							if (stack_p<stack_begin){
								Int extra_words;

								extra_words=stack_begin-stack_p;
								n_free_words-=extra_words;
								if (n_free_words<0){
									*last_heap_pa=heap_p+arity+1+(stack_end-stack_p);
									return (Int*)((Int)&string_p[1]+1);
								}
								stack_begin=stack_p;
							}

							arg_a=(Int**)&heap_p[1];
							heap_p+=2;
							
							--n_words;
							stack_p[n_words]=(Int**)&heap_p[n_words];
							while (--n_words>=0)
								stack_p[n_words]=(Int**)&heap_p[n_words];

							++string_p;
							heap_p+=arity-1;
							continue;
						} else if (arity!=257){
							Int n_pointers,n_non_pointers,*non_pointers_p;
							
							n_non_pointers=arity>>8;
							arity=arity & 255;
							n_pointers=arity - n_non_pointers;

							n_free_words-=arity;
							if (n_free_words<0){
								*last_heap_pa=heap_p+arity+1+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}

							++string_p;
							++heap_p;
							non_pointers_p=&heap_p[n_pointers];

							copy (non_pointers_p,string_p,n_non_pointers);
							string_p+=n_non_pointers;
							
							if (n_pointers==0){
								heap_p+=arity;
								break;
							} else {
								arg_a=(Int**)&heap_p[0];
								++heap_p;
								if (n_pointers>1){
									--n_pointers;
									
									stack_p-=n_pointers;
									if (stack_p<stack_begin){
										Int extra_words;

										extra_words=stack_begin-stack_p;
										n_free_words-=extra_words;
										if (n_free_words<0){
											*last_heap_pa=heap_p+arity+(stack_end-stack_p);
											return (Int*)((Int)string_p+1);
										}
										stack_begin=stack_p;
									}
									
									--n_pointers;
									stack_p[n_pointers]=(Int**)&heap_p[n_pointers];
									while (--n_pointers>=0)
										stack_p[n_pointers]=(Int**)&heap_p[n_pointers];
								}
								heap_p+=arity-1;
								continue;
							}
						} else {
							n_free_words-=2;
							if (n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-stack_begin);
								return (Int*)((Int)&string_p[1]+1);
							}

							heap_p[1]=string_p[1];
							string_p+=2;
							heap_p+=3;
							break;
						}
					} else {
						n_free_words-=2;
						if (n_free_words<0){
							*last_heap_pa=heap_p+3+(stack_end-stack_begin);
							return (Int*)((Int)&string_p[1]+1);
						}
					
						++string_p;
						if (arity==0){
							heap_p+=3;
							break;
						} else {
							arg_a=(Int**)&heap_p[1];
							heap_p+=3;
							continue;
						}
					}
				}
			} else {
				Int *node_p;

#ifdef THREAD
				if (desc & 2){
# ifdef USE_DESC_RELATIVE_TO_ARRAY
					node_p=(Int*)(desc-3)+(Int)&__ARRAY__;
# else
					node_p=(Int*)(desc-3);
# endif
					*arg_a=node_p;
					++string_p;
					break;
				}
#endif
				
				node_p=*(Int**)((Int)string_p+(desc-1));
				*arg_a=node_p;
				++string_p;
				break;
			}
		}

		if (stack_p==stack_end)
			break;
		
		arg_a=*stack_p++;
	}
	
	*last_heap_pa=heap_p;
	return root_node_p;
}

void remove_forwarding_pointers_from_string (Int *string_p,Int *end_forwarding_pointers)
{
	string_p+=2;

	while (string_p<end_forwarding_pointers){
		Int forwarding_pointer;
			
		forwarding_pointer=*string_p;
		if (!(forwarding_pointer & 1)){
			Int desc;
			
			desc=*(Int*)forwarding_pointer;
#ifdef USE_DESC_RELATIVE_TO_ARRAY
			*string_p=desc-(Int)&__ARRAY__;
#else
			*string_p=desc;
#endif
			if (desc & 2){
				unsigned Int arity;
				
				arity=((unsigned short *)desc)[-1];
				if (arity==0){
					if (desc==(Int)&INT_descriptor+2 || desc==(Int)&CHAR+2 || desc==(Int)&BOOL+2
#if ARCH_64
						|| desc==(Int)&REAL+2
#endif
					){
						string_p+=2;
#if ! ARCH_64
					} else if (desc==(Int)&REAL+2){
						string_p+=3;
#endif
					} else if (desc==(Int)&__STRING__+2){
						unsigned Int length,n_words;
							
						length=string_p[1];
						string_p+=2;
#if ARCH_64
						n_words=(length+7)>>3;
#else
						n_words=(length+3)>>2;
#endif
						string_p+=n_words;
					} else if (desc==(Int)&__ARRAY__+2){
						Int array_size,elem_desc;

						array_size=string_p[1];
						elem_desc=string_p[2];
						string_p+=3;

						if (elem_desc==0){
						} else if (elem_desc==(Int)&INT_descriptor+2
#if ARCH_64
							|| elem_desc==(Int)&REAL+2
#endif
						){
							string_p+=array_size;
#if ! ARCH_64
						} else if (elem_desc==(Int)&REAL+2){
							array_size<<=1;
							string_p+=array_size;
#endif
						} else if (elem_desc==(Int)&BOOL+2){
#if ARCH_64
							array_size=(array_size+7)>>3;
#else
							array_size=(array_size+3)>>2;
#endif
							string_p+=array_size;
						} else {
							Int n_field_pointers,n_non_field_pointers,field_size;

							n_field_pointers=*(unsigned short *)elem_desc;
							field_size=((unsigned short *)elem_desc)[-1]-(Int)256;
							n_non_field_pointers=field_size-n_field_pointers;
						
							string_p+=n_non_field_pointers*array_size;
						}
					} else {
						++string_p;
					}
				} else {
					++string_p;
					if (arity>=256){
						Int n_pointers,n_non_pointers;

						n_pointers=*(unsigned short*)desc;
						arity-=256;
						n_non_pointers=arity-n_pointers;
						string_p+=n_non_pointers;
					}
				}
			} else {
				Int arity;

				arity=((int*)desc)[-1];
				++string_p;
				if (arity>=256){
					Int n_non_pointers;
					
					n_non_pointers=arity>>8;
					string_p+=n_non_pointers;
				}
			}
		} else {
			++string_p;
		}
	}
}
