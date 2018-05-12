
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

int is_using_desc_relative_to_array()
{
#ifdef USE_DESC_RELATIVE_TO_ARRAY
	return 1;
#else
	return 0;
#endif
}

int size_element_descriptor_currying()
{
#if defined (MACH_O64)
	return 16;
#else
	return 8;
#endif
}

extern void *INT_descriptor,*CHAR,*BOOL,*REAL,*__STRING__,*__ARRAY__;

/*inline*/
static void copy (Int *dest_p,Int *source_p,Int n_words)
{
	Int i;

	for (i=0; i<n_words; ++i)
		dest_p[i]=source_p[i];
}

Int *copy_graph_to_string (Int *node_p,void *begin_free_heap,void *end_free_heap
#ifdef THREAD
							,void *begin_heap,unsigned Int heap_size
#endif
							)
{
	Int **stack_p,**stack_begin,**stack_end,*heap_p;

	stack_end=end_free_heap;
	stack_begin=end_free_heap;
	stack_p=end_free_heap;

	heap_p=begin_free_heap;

	if (heap_p+2>(Int*)stack_begin)
		return NULL;

	heap_p[0]=(Int)&__STRING__+2;
	heap_p[1]=0;
	heap_p+=2;

	for (;;){
		for (;;){
			Int desc;

#ifdef THREAD
			if (((unsigned Int)node_p-(unsigned Int)begin_heap)>=heap_size){
				if (heap_p>=(Int*)stack_begin)
					return NULL;
# ifdef USE_DESC_RELATIVE_TO_ARRAY
				heap_p[0]=3+(Int)node_p-(Int)&__ARRAY__;
# else
				heap_p[0]=3+(Int)node_p;
# endif
				++heap_p;
				break;
			}
#endif

			desc=*node_p;

			if (heap_p>=(Int*)stack_begin)
				return NULL;
			
			if (!(desc & 1)){
				*node_p=1+(Int)heap_p;
#ifdef USE_DESC_RELATIVE_TO_ARRAY
				*heap_p++=desc-(Int)&__ARRAY__;
#else
				*heap_p++=desc;
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
							if (heap_p>=(Int*)stack_begin)
								return NULL;

							*heap_p++=node_p[1];
							break;
#if ! ARCH_64
						} else if (desc==(Int)&REAL+2){
							if (heap_p+2>(Int*)stack_begin)
								return NULL;
						
							heap_p[0]=node_p[1];
							heap_p[1]=node_p[2];
							heap_p+=2;
							break;
#endif
						} else if (desc==(Int)&__STRING__+2){
							unsigned Int length,n_words;
							
							length=node_p[1];
							node_p+=2;
#if ARCH_64
							n_words=(length+7)>>3;							
#else
							n_words=(length+3)>>2;
#endif							
							if (heap_p+n_words>=(Int*)stack_begin)
								return NULL;
													
							*heap_p++=length;

							copy (heap_p,node_p,n_words);
							heap_p+=n_words;
							break;
						} else if (desc==(Int)&__ARRAY__+2){
							Int array_size,elem_desc;

							if (heap_p+2>(Int*)stack_begin){
								*node_p=desc;
								return NULL;
							}

							array_size=node_p[1];
							elem_desc=node_p[2];
							node_p+=3;
		
							heap_p[0]=array_size;
#if defined (MACH_O64)
							heap_p[1]=elem_desc==0 ? elem_desc : elem_desc-(Int)&__ARRAY__;
#else
							heap_p[1]=elem_desc;
#endif
							heap_p+=2;
													
							if (elem_desc==0){
								stack_p-=array_size;
								if (stack_p<stack_begin){
									if ((Int*)stack_p<heap_p){
										node_p[-3]=desc;
										return NULL;
									}
									stack_begin=stack_p;
								}
								
								while (--array_size>=0)
									stack_p[array_size]=(Int*)node_p[array_size];						
								break;
							} else if (elem_desc==(Int)&INT_descriptor+2
#if ARCH_64
								|| elem_desc==(Int)&REAL+2
#endif
							){
								if (heap_p+array_size>(Int*)stack_begin)
									return NULL;
							
								copy (heap_p,node_p,array_size);	
								heap_p+=array_size;
								break;
#if ! ARCH_64
							} else if (elem_desc==(Int)&REAL+2){
								array_size<<=1;
							
								if (heap_p+array_size>(Int*)stack_begin)
									return NULL;
								
								copy (heap_p,node_p,array_size);
								heap_p+=array_size;
								break;
#endif
							} else if (elem_desc==(Int)&BOOL+2){
#if ARCH_64
								array_size=(array_size+7)>>3;
#else
								array_size=(array_size+3)>>2;
#endif
								if (heap_p+array_size>(Int*)stack_begin)
									return NULL;
								
								copy (heap_p,node_p,array_size);
								heap_p+=array_size;
								break;
							} else {
								Int n_field_pointers,field_size;
								
								n_field_pointers=*(unsigned short *)elem_desc;
								field_size=((unsigned short *)elem_desc)[-1]-(Int)256;

								if (n_field_pointers==0){
									array_size*=field_size;
									
									if (heap_p+array_size>(Int*)stack_begin){
										node_p[-3]=desc;
										return NULL;
									}
									
									copy (heap_p,node_p,array_size);
									heap_p+=array_size;
									break;
								} else if (n_field_pointers==field_size){
									array_size*=field_size;

									stack_p-=array_size;
									if (stack_p<stack_begin){
										if ((Int*)stack_p<heap_p){
											node_p[-3]=desc;
											return NULL;
										}
										stack_begin=stack_p;
									}
									
									while (--array_size>=0)
										stack_p[array_size]=(Int*)node_p[array_size];						
									break;
								} else {
									Int n_non_field_pointers,n_array_pointers,n_array_non_pointers,i,*pointer_p;
									
									n_non_field_pointers=field_size-n_field_pointers;
									n_array_pointers=n_field_pointers*array_size;
									n_array_non_pointers=n_non_field_pointers*array_size;
									
									if (heap_p+n_array_non_pointers>(Int*)stack_begin){
										node_p[-3]=desc;
										return NULL;
									}
									
									stack_p-=n_array_pointers;
									if (stack_p<stack_begin){
										if ((Int*)stack_p<heap_p+n_array_non_pointers){
											node_p[-3]=desc;
											return NULL;
										}
										stack_begin=stack_p;
									}
									
									pointer_p=(Int*)stack_p;
									
									for (i=0; i<array_size; ++i){
										copy (pointer_p,node_p,n_field_pointers);
										pointer_p+=n_field_pointers;
										node_p+=n_field_pointers;
										
										copy (heap_p,node_p,n_non_field_pointers);
										heap_p+=n_non_field_pointers;
										node_p+=n_non_field_pointers;						
									}
									break;
								}
							}
						} else {
							break;
						}				
					} else if (arity==1){
						node_p=(Int*)node_p[1];
						continue;
					} else if (arity==2){
						if (stack_p<=stack_begin){
							if ((Int*)stack_p<=heap_p){
								*node_p=desc;
								return NULL;
							}
							--stack_begin;
						}

						*--stack_p=(Int*)node_p[2];
						node_p=(Int*)node_p[1];
						continue;
					} else if (arity<256){
						Int **args,n_words;

						args=(Int**)node_p[2];
						n_words=arity-1;
						
						stack_p-=n_words;
						if (stack_p<stack_begin){
							if ((Int*)stack_p<heap_p){
								*node_p=desc;
								return NULL;
							}
							stack_begin=stack_p;
						}
						
						--n_words;
						stack_p[n_words]=args[n_words];
						--n_words;
						stack_p[n_words]=args[n_words];
						while (--n_words>=0)
							stack_p[n_words]=args[n_words];

						node_p=(Int*)node_p[1];
						continue;
					} else {
						Int n_pointers;
						
						n_pointers=*(unsigned short*)desc;
						arity-=256;
						
						if (arity==1){
							if (n_pointers==0){
								if (heap_p>=(Int*)stack_begin)
									return NULL;
						
								*heap_p++=node_p[1];
								break;
							} else {
								node_p=(Int*)node_p[1];
								continue;
							}
						} else if (arity==2){
							if (n_pointers==0){
								if (heap_p+2>(Int*)stack_begin)
									return NULL;
							
								heap_p[0]=node_p[1];
								heap_p[1]=node_p[2];
								heap_p+=2;
								break;
							} else {
								if (n_pointers==1){
									if (heap_p>=(Int*)stack_begin)
										return NULL;
						
									*heap_p++=node_p[2];
								} else {
									if (stack_p<=stack_begin){
										if ((Int*)stack_p<=heap_p){
											*node_p=desc;
											return NULL;
										}
										--stack_begin;
									}

									*--stack_p=(Int*)node_p[2];
								}
								node_p=(Int*)node_p[1];
								continue;							
							}
						} else {
							Int *args;

							args=(Int*)node_p[2];

							if (n_pointers==0){
								if (heap_p+arity>=(Int*)stack_begin)
									return NULL;

								heap_p[0]=node_p[1];
								++heap_p;
								--arity;

								copy (heap_p,args,arity);
								heap_p+=arity;
								break;
							} else {
								Int n_non_pointers;

								n_non_pointers=arity-n_pointers;

								if (n_non_pointers>0){
									Int *non_pointers_p;

									if (heap_p+n_non_pointers>(Int*)stack_begin)
										return NULL;

									non_pointers_p=&args[n_pointers-1];

									copy (heap_p,non_pointers_p,n_non_pointers);
									heap_p+=n_non_pointers;
								}
								
								--n_pointers;
								if (n_pointers>0){
									stack_p-=n_pointers;
									if (stack_p<stack_begin){
										if ((Int*)stack_p<heap_p){
											*node_p=desc;
											return NULL;
										}
										stack_begin=stack_p;
									}
									
									copy ((Int*)stack_p,args,n_pointers);
								}

								node_p=(Int*)node_p[1];
								continue;
							}
						}
					}
				} else {
					Int arity;

					arity=((int*)desc)[-1];
					if (arity>1){
						if (arity<256){
							Int **args,n_words;
			
							args=(Int**)&node_p[2];
							n_words=arity-1;
							
							stack_p-=n_words;
							if (stack_p<stack_begin){
								if ((Int*)stack_p<heap_p){
									*node_p=desc;
									return NULL;
								}
								stack_begin=stack_p;
							}
							
							--n_words;
							stack_p[n_words]=args[n_words];
							while (--n_words>=0)
								stack_p[n_words]=args[n_words];
							
							node_p=(Int*)node_p[1];
							continue;
						} else {
							Int n_pointers,n_non_pointers,*non_pointers_p;
							
							n_non_pointers=arity>>8;
							n_pointers=(arity & 255) - n_non_pointers;
							
							if (heap_p+n_non_pointers>(Int*)stack_begin)
								return NULL;
							
							non_pointers_p=&node_p[1+n_pointers];

							copy (heap_p,non_pointers_p,n_non_pointers);
							heap_p+=n_non_pointers;
							
							if (n_pointers==0)
								break;
							else {
								if (n_pointers>1){
									Int **args;
									
									args=(Int**)&node_p[2];
									--n_pointers;
									
									stack_p-=n_pointers;
									if (stack_p<stack_begin){
										if ((Int*)stack_p<heap_p){
											*node_p=desc;
											return NULL;
										}
										stack_begin=stack_p;
									}
									
									--n_pointers;
									stack_p[n_pointers]=args[n_pointers];
									while (--n_pointers>=0)
										stack_p[n_pointers]=args[n_pointers];
								}
								node_p=(Int*)node_p[1];
								continue;
							}
						}
					} else if (arity==0){
						break;
					} else {
						node_p=(Int*)node_p[1];
						continue;
					}
				}
			} else {
				if (heap_p>=(Int*)stack_begin)
					return NULL;
				
				heap_p[0]=1+(desc-1)-(Int)heap_p;
				++heap_p;
				break;
			}
		}

		if (stack_p==stack_end){
			((Int*)begin_free_heap)[1]=(Int)heap_p-(Int)begin_free_heap-(2*sizeof(Int));
			return begin_free_heap;
		}
		
		node_p=*stack_p++;
	}
	
	return NULL;
}

void remove_forwarding_pointers_from_graph (Int *node_p,Int **stack_end)
{
	Int **stack_p;

	stack_p = stack_end;

	for (;;){
		for (;;){
			Int forwarding_pointer,desc;
			
			forwarding_pointer=*node_p;
			if ((forwarding_pointer & 1)==0)
				break;

#ifdef USE_DESC_RELATIVE_TO_ARRAY
			desc = (Int)&__ARRAY__ + *((Int*)(forwarding_pointer-1));
#else
			desc = *((Int*)(forwarding_pointer-1));
#endif
			*node_p=desc;
			
			if (desc & 2){
				unsigned Int arity;
					
				arity=((unsigned short *)desc)[-1];
				if (arity==0){
					if (desc!=(Int)&__ARRAY__+2){
						break;
					} else {
						Int elem_desc;

						elem_desc=node_p[2];

						if (elem_desc==0){
							Int array_size;
							
							array_size=node_p[1];
							node_p+=3;
							
							stack_p-=array_size;
							
							while (--array_size>=0)
								stack_p[array_size]=(Int*)node_p[array_size];						
							break;
						} else if (elem_desc==(Int)&INT_descriptor+2 || elem_desc==(Int)&REAL+2 || elem_desc==(Int)&BOOL+2){
							break;
						} else {
							Int n_field_pointers;
							
							n_field_pointers=*(unsigned short *)elem_desc;

							if (n_field_pointers!=0){
								Int field_size,array_size;
								
								field_size=((unsigned short *)elem_desc)[-1]-(Int)256;

								array_size=node_p[1];
								node_p+=3;
						
								if (n_field_pointers==field_size){
									array_size*=field_size;

									stack_p-=array_size;
									
									while (--array_size>=0)
										stack_p[array_size]=(Int*)node_p[array_size];						
								} else {
									Int n_array_pointers,i,*pointer_p;
									
									n_array_pointers=n_field_pointers*array_size;
									
									stack_p-=n_array_pointers;
									
									pointer_p=(Int*)stack_p;
									
									for (i=0; i<array_size; ++i){
										copy (pointer_p,node_p,n_field_pointers);
										pointer_p+=n_field_pointers;
										node_p+=field_size;
									}
								}
							}
							break;
						}						
					}
				} else if (arity==1){
					node_p=(Int*)node_p[1];
					continue;
				} else if (arity==2){
					*--stack_p=(Int*)node_p[2];
				
					node_p=(Int*)node_p[1];
					continue;
				} else if (arity<256){
					Int **args,n_words;

					args=(Int**)node_p[2];
					n_words=arity-1;
					
					stack_p-=n_words;
					
					--n_words;
					stack_p[n_words]=args[n_words];
					while (--n_words>=0)
						stack_p[n_words]=args[n_words];

					node_p=(Int*)node_p[1];
					continue;					
				} else {
					Int n_pointers;
					
					n_pointers=*(unsigned short*)desc;
					if (n_pointers==0)
						break;
					else {
						if (n_pointers>=2){
							if (n_pointers==2){
								arity-=256;
								if (arity==2){
									*--stack_p=(Int*)node_p[2];									
								} else {
									Int **args;

									args=(Int**)node_p[2];
									*--stack_p=args[0];									
								}
							} else {
								Int **args,n_words;

								args=(Int**)node_p[2];
								n_words=n_pointers-1;
								
								stack_p-=n_words;
								
								--n_words;
								stack_p[n_words]=args[n_words];
								while (--n_words>=0)
									stack_p[n_words]=args[n_words];

							}
						}
						node_p=(Int*)node_p[1];
						continue;
					}
				}
			} else {
				Int arity;

				arity=((int*)desc)[-1];
				if (arity>1){
					if (arity<256){
						Int **args,n_words;
		
						args=(Int**)&node_p[2];
						n_words=arity-1;
						
						stack_p-=n_words;
						
						--n_words;
						stack_p[n_words]=args[n_words];
						while (--n_words>=0)
							stack_p[n_words]=args[n_words];
						
						node_p=(Int*)node_p[1];
						continue;
					} else {
						Int n_pointers,n_non_pointers;
						
						n_non_pointers=arity>>8;
						n_pointers=(arity & 255) - n_non_pointers;
												
						if (n_pointers==0)
							break;
						else {
							if (n_pointers>1){
								Int **args;
								
								args=(Int**)&node_p[2];
								--n_pointers;
								
								stack_p-=n_pointers;
								
								--n_pointers;
								stack_p[n_pointers]=args[n_pointers];
								while (--n_pointers>=0)
									stack_p[n_pointers]=args[n_pointers];
							}
							node_p=(Int*)node_p[1];
							continue;
						}						
					}
				} else if (arity==0){
					break;
				} else {
					node_p=(Int*)node_p[1];
					continue;
				}
			}
		}

		if (stack_p==stack_end)
			return;
		
		node_p=*stack_p++;
	}	
}
