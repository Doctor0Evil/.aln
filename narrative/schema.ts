// narrative/schema.ts
export interface Gate { id:string; expr:(s:any)=>boolean }
export interface Branch { id:string; weight:number; next:string }
export interface Node {
  id:string; gates:Gate[]; branches:Branch[];
  onEnter?:(s:any)=>void; onExit?:(s:any)=>void;
}
