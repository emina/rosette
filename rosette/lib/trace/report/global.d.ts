interface IEntry {
  type: string,
  data: any,
}

interface IStxInfo {
  stx: string,
  srcloc: ISrcloc
}

interface ISrcloc {
  line: number,
  column: number,
  source: string,
}

interface IExnTrace {
  name?: string,
  srcloc?: ISrcloc,
}

interface ICallStack {
  name: string,
  srcloc: ISrcloc,
}

interface ITraceEntrySingle {
  kind: 'no-group',
  key: string,
  exn_msg: string,
  stx_info?: IStxInfo,

  timestamp: number,
  exn_trace: IExnTrace[],
  call_stack: ICallStack[],
}

interface IGroupDetail {
  key: string,
  timestamp: number,
  exn_trace: IExnTrace[],
  call_stack: ICallStack[],
  full_exn_msg: string,
}

interface ITraceEntryGroup {
  kind: 'group',
  key: string,
  exn_msg: string,
  stx_info?: IStxInfo,

  group: IGroupDetail[],
}

type ITraceEntry = ITraceEntrySingle | ITraceEntryGroup;